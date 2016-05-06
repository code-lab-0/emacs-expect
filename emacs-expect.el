;;; -*- lexical-binding: t -*-

;;; Emacs 24 has optional lexical binding, 
;;; which can be enabled on a per-buffer basis.
;;; http://rubikitch.com/tag/require-cl%E5%95%8F%E9%A1%8C/  (in Japanese)


;; -------------------------------------------------------------
;; Emacs expect -- Automatic and asynchronous operation of terminals.


;;; Example Usage:
;;
;; (ee:clear-queue)
;;
;; (ee:command "*shell*"  "\\$ $"  "ssh 192.168.1.7")
;; (ee:password "*shell*" "password: $" "azure")
;; (ee:command "*shell*"  "\\$ $" "sudo apt-get update")
;; (ee-c:commands "*shell*" "sudo password"
;; 			 '(
;; 			  '("\\$ $" "")
;; 			  '("password for [a-z]+:\\s+$" "azure" t)
;; 			  ))


(require 'queue)
(require 'deferred)
(require 'dash)
(require 'subr-x)
(require 'pcre2el)


;;; This expression make Emacs echo passwords in shell mode buffers,
;;; here this is evaluated in order to be able to send password automatically. 
(remove-hook 'comint-output-filter-functions
			 'comint-watch-for-password-prompt)



;;; ==============================
;;;
;;;   Job Queue
;;;
;;; ==============================


;;; eq:queue is a hash table which keeps information of
;;; buffer-name => a queue of (list desc pred action) elements.
(set 'ee:queue (make-hash-table :test #'equal))


(defun ee:submit (buffer desc pred action)
  "This function submit a job to the job queue (ee:queue)."
  (if (not (gethash buffer ee:queue))
	  (puthash buffer (make-queue) ee:queue))

  (queue-enqueue (gethash buffer ee:queue) (list desc pred action)))


(defun ee:clear-queue ()
  (clrhash ee:queue))


(defun ee:print-queue ()
  (dolist (buffer (hash-table-keys ee:queue))
	(insert "\n")
	(insert (ee:print-qelems buffer))))


(defun ee:print-qelems (buffer)
  (let* ((elem-list (queue-all (gethash buffer ee:queue))))
	(dolist (elem elem-list)
	  (insert
	   (concat buffer "\t" (car elem) "\n")))))



(defun ee:queue-length ()
  (let ((sum
		 (-reduce
		  '+
		  (-map 
		   (lambda (buf) 
			 (if (queue-empty (gethash buf ee:queue)) 0 1))
		   (hash-table-keys ee:queue)))))

	sum))



;;; ==============================
;;;
;;;   Job Execution
;;;
;;; ==============================

(setq ee:running-p nil)

(defun ee:start ()
  (setq ee:running-p t)
  (deferred:$
	(deferred:next
	  (lambda (x) (princ "ee is started")))
	(deferred:nextc it
	  (deferred:lambda (x)
		(deferred:$
		  (deferred:next
			(lambda ()
			  
			  (dolist (buffer (hash-table-keys ee:queue))
				(let* ((q (gethash buffer ee:queue))
					   (elem (if q (queue-first q) nil)))

				  (ee:eval-qelem elem)))))

		  ;; (deferred:nextc it
		  ;; 	(lambda ()
		  ;; 	  (princ "---")))		  
		  (deferred:nextc it
			(lambda () 
			  (deferred:process "sh" "-c" "sleep 1")))
		  ;; (deferred:nextc it
		  ;; 	(lambda ()
		  ;; 	  (princ "***")))
		  (deferred:nextc it
			(lambda () 
			  (if (= (ee:queue-length) 0)
				  (setq ee:running-p nil))))
		  (if ee:running-p
			  (deferred:nextc it self)
			(progn 
			  (princ "ee is stopped.")
			  nil))
		  )))))


(defun ee:stop ()
  (setq ee:running-p nil))


(defun ee:init ()
  (ee:stop)
  (ee:clear-queue))


(defun ee:eval-qelem (qelem)
  (let* ((desc (car qelem))
		(pred (car (cdr qelem)))
		(action (car (cddr qelem)))
		(result (if pred (funcall pred) nil)))

	(if result (funcall action))
	result))



;;; ========================================
;;;
;;;   Job submission utilities
;;;   (1) Simple submission.
;;;
;;; ========================================

;;; predicates and actions.

(defun ee:pred:trivial ()
  (lambda () t))


(defun ee:pred:prompt (buffer prompt)
  (lambda ()
	(string-match
	 (rxt-pcre-to-elisp prompt)
	 (ee:tail-chars 100 buffer))))


(defun ee:action:command (buffer command)
  (lambda ()
	(ee:send-input buffer command)
	(queue-dequeue (gethash buffer ee:queue))))


(defun ee:action:password (buffer host)
  (lambda ()
	(set-buffer buffer)
	(comint-send-string buffer (concat (ee:get-password host) "\n"))
	(queue-dequeue (gethash buffer ee:queue))))


;;; job submission utilities.

(defun ee:submit-command (buffer prompt command)
  (ee:submit buffer
			 command
			 (ee:pred:prompt buffer prompt)
			 (ee:action:command buffer command)))


(defun ee:submit-password (buffer prompt host)
  (ee:submit buffer
			 "********"
			 (ee:pred:prompt buffer prompt)
			 (ee:action:password buffer host)))


;;; submit and start functions.

(defun ee:command (buffer prompt command)
  (ee:submit-command buffer prompt command)
  (if (not ee:running-p)
	  (ee:start)))


(defun ee:password (buffer prompt host)
  (ee:submit-password buffer prompt  host)
  (if (not ee:running-p)
	  (ee:start)))





;;; ========================================
;;;
;;;   Job submission utilities
;;;   (2) simple automaton
;;;
;;; ========================================

;;; Here, an automaton consists of
;;; 1. (pred action) list
;;; 2. current state
;;; 3. In general, we also need start state and end (accept) state.

;;; predicates and actions.
(set 'ee-c:automaton-state (make-hash-table :test #'equal))

(defun ee-c:action:init-automaton (buffer)
  (lambda ()
	(puthash buffer 0 ee-c:automaton-state)
	(queue-dequeue (gethash buffer ee:queue))))

(defun ee-c:action:command (buffer command state list-length)
  (lambda ()
	(ee:send-input buffer command)
	(ee-c:next-state buffer state list-length)))


(defun ee-c:action:password (buffer host state list-length)
  (lambda ()
	(set-buffer buffer)
	(comint-send-string buffer (concat (ee:get-password host) "\n"))
	(ee-c:next-state buffer state list-length)))


(defun ee-c:action:dequeue (buffer)
  (lambda ()
	(queue-dequeue (gethash buffer ee:queue))))


(defun ee-c:next-state (buffer state list-length)
	(let* ((ns (+ state 1))
		   (next-state (if (>= ns list-length) 0 ns)))
	  (puthash buffer next-state ee-c:automaton-state)))


(defun ee-c:get-state (buffer)
  (gethash buffer ee-c:automaton-state))


(defun ee-c:create-automaton (buffer prompt-command-list)
  (let* ((list-length (length prompt-command-list))
		 (queue (make-queue)))

	(dotimes (i list-length)
	  (let* ((pair (cadr (nth i prompt-command-list)))
			 (pred (car pair))
			 (command (cadr pair))
			 (invisible-p (car (cddr pair))))

		(if invisible-p
			(queue-enqueue queue
						   (list
							(ee:pred:prompt buffer pred)
							(ee-c:action:password buffer command i list-length)))
	  
		  (queue-enqueue queue
						 (list
						  (ee:pred:prompt buffer pred)
						  (ee-c:action:command buffer command i list-length))))
		))

	(queue-all queue)))


   
(defun ee-c:pred:accept (buffer machine)
  (lambda ()
	(let* ((state (ee-c:get-state buffer))
		   (len   (length machine))
		   (elem  (nth state machine))
		   (pred  (car elem))
		   (action (cadr elem))
		   (pred-result (funcall pred))
		   (result (if (and (= state 0)) pred-result)))

	  (if pred-result
		  (funcall action)
		(ee-c:next-state buffer state len))

	  result)))


;;; job submission utilities.


(defun ee-c:submit-commands (buffer desc list-of-pairs)
  (let ((machine (ee-c:create-automaton buffer list-of-pairs)))
	(ee:submit buffer (concat desc "(init)")
			   (ee:pred:trivial)
			   (ee-c:action:init-automaton buffer))
	(ee:submit buffer desc
			   (ee-c:pred:accept buffer machine)
			   (ee-c:action:dequeue buffer))))



(defun ee-c:commands (buffer desc list-of-pairs)
  (ee-c:submit-commands buffer desc list-of-pairs)
  (if (not ee:running-p)
	  (ee:start)))
	

;;; ========================================
;;;
;;;   Job submission utilities
;;;   (3) Yet another job submission function.
;;;
;;; ========================================

(defun ee (buffer job-list)
  (dolist (job job-list)
	(cond ((stringp job) (ee:command buffer "\\$ $" job))
		  ((listp job) (funcall (eval job))))))



;;; ==============================
;;;
;;;   Buffer read/write utilities.
;;;
;;; ==============================


(defun ee:tail-chars (num-chars buffer)
  (set-buffer buffer)
  (buffer-substring-no-properties 
   (max (- (point-max) 100) (point-min)) 
   (point-max) ))


(defun ee:send-input (buffer command)
  (set-buffer buffer)
  (insert command)
  (comint-send-input))

;;;
;;;   password
;;;

(set 'ee:password (make-hash-table :test #'equal))

(defun ee:get-password (name)
  (gethash name ee:password))


(defun ee:send-password (buffer host)
  (set-buffer buffer)
  (comint-send-string buffer (concat (ee:get-password host) "\n")))


;;; ---

;;; http://ergoemacs.org/emacs/elisp_read_file_content.html
(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
	(insert-file-contents filePath)
	    (split-string (buffer-string) "\n" t)))


(defun ee:read-password (filePath)
  (dolist (line (read-lines filePath))
	(let* ((kv (split-string line "\t" t))
		   (key (car kv))
		   (value (cadr kv)))
	  (puthash key value ee:password)))
  )

(ee:read-password "~/.emacs.d/ee-path.txt")





;;; ======================================
;;;
;;;   Execution : Higher level functions
;;;
;;; ======================================


;; (defun ee:run (buffer command-list)
;;   (dolist (item command-list)
;; 	(cond ((stringp item) (ee buffer "\\$ $" item))
;; 		  ((listp item) (ee:send-password buffer item)))))



;; (defun ee (buffer prompt command &optional invisible-p)
;;   (ee:submit buffer prompt command invisible-p)
;;   (if (not ee:running-p)
;; 	  (ee:start)))



;; (defun ee:send-password (buffer info)
;;   (let* ((prompt (car info))
;; 		 (p (car (cdr info)))
;; 		 (password (if (listp p) (eval p) p))
;; 		 (invisible-p (car (cddr info))))
;; 	(ee buffer prompt password invisible-p)))

	
  

	
  







;;; ==============================
;;;
;;;   Perspectives
;;;
;;; ==============================

(defun ee:persp ()
  (interactive)
  (progn
	(delete-other-windows)
	(let ((h (ceiling (* (window-height) 0.7)))
		  (w (ceiling (* (window-width) 0.6))))
	  (split-window 'nil w t)
	  (split-window 'nil h 'nil)
	  (other-window 3))

	(setq ee:window-list (window-list))
	
	(progn
	  (other-window 1)
	  (switch-to-buffer "*scratch*")
	  (other-window 1)
	  (shell)
	  (other-window 1)
	  )))


(defun ee:persp2 ()
  (interactive)
  (progn 
	(delete-other-windows)
	(let ((h1 (ceiling (* (window-height) 0.5)))
		  (h2 (ceiling (* (window-height) 0.7)))
		  (w (ceiling (* (window-width) 0.6))))
	  (split-window 'nil w t)
	  (split-window 'nil h1 'nil)
	  (select-window (car (cddr (window-list))))
	  (split-window 'nil h2 'nil)
	  (other-window 2))

	(setq ee:window-list (window-list))
	
	(progn 
	  (other-window 2)
	  (shell)
	  (other-window 1)
	  (switch-to-buffer "*scratch*")
	  (other-window 1))))


(defun ee:open-shell-buffer (buf-name)
  (interactive "sShell buffer name: \n")
  (progn
	(other-window 1)
	(shell)
	(rename-buffer buf-name)
	(other-window 2)))

;;; ---

(defun ee:open-on-buffer1 (buffer-name)
  (interactive "sbuffer-name: \n")
  (select-window (car ee:window-list))
  (switch-to-buffer buffer-name))


(defun ee:open-on-buffer2 (buffer-name)
  (interactive "sbuffer-name: \n")
  (select-window (car (cdr ee:window-list)))
  (switch-to-buffer buffer-name))


(defun ee:open-on-buffer3 (buffer-name)
  (interactive "sbuffer-name: \n")
  (select-window (car (cdr (cdr ee:window-list))))
  (switch-to-buffer buffer-name))


(defun ee:open-on-buffer4 (buffer-name)
  (interactive "sbuffer-name: \n")
  (select-window (car (cdr (cddr ee:window-list))))
  (switch-to-buffer buffer-name))


(defun ee:shell-buffer-list ()
  (hash-table-keys ee:queue))



;;; ----------

(provide 'emacs-expect)

