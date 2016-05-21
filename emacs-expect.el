;;; -*- lexical-binding: t -*-

;;; Emacs 24 has optional lexical binding, 
;;; which can be enabled on a per-buffer basis.

;; -------------------------------------------------------------------------
;; Emacs expect -- Asynchronous automatic operation of Emacs Shell buffers. 

(defconst emacs-expect-version "1.0")

;; Copyright (C) 2016 Osamu Ogasawara

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;; --------------------------------------------------------------------------
;; Installation
;;

;; 1. Prerequisites
;;
;; - GNU Emacs ver 24.5 or higher.
;; - GnuPG
;;   - (for Mac OS X, https://gpgtools.org/ )
;;


;; 2. Install following packages on which the Emacs-Expect depends.

;; Queue data structure
(require 'queue)

;; Asynchronous task management 
;; https://github.com/kiwanami/emacs-deferred
(require 'deferred) 

;; A modern list api for Emacs.
;; https://github.com/magnars/dash.el
(require 'dash)

;; Extra lisp functions 
(require 'subr-x)

;; PCRE to Emacs regular expression conversion
;; https://github.com/joddie/pcre2el
(require 'pcre2el)

;; These packages can be installed with package.el which is bundled in Emacs 24 or higher.
;; Before you install these packages, you need to add Marmalade and Melpa
;; archives to the search list as follows.
;;
;; (require 'package)
;; (add-to-list 'package-archives
;; 			 '("marmalade" . "http://marmalade-repo.org/packages/"))
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; (package-initialize)


;; 3. Prepareing an encrypted password file.
;;
;; You need to make a ~/.emacs.d/ee-pass.txt file
;; which consists of tab-delimited name and password pairs.
;;
;; --------- ee-pass.txt ------------
;; server01    password-of-server01
;; server02    password-of-server02
;; ...
;; ----------------------------------
;; 
;; After creating this file, open this file with Emacs,
;; then encrypt it with M-x epa-encrypt-file.


;;; ----------------------------------------------------------------------------
;;; Example Usage:

;; 1. A series of logging-in, virtual environment setting-up.
;; (ee:persp)
;; (ee:init)
;; (ee:open-shell-buffer "*shell*(nig)")
;; (setq buf "*shell*(nig)")
;;
;; (ee:command buf  "\\$ $"  "ssh -X gw2.ddbj.nig.ac.jp")
;; (ee:command buf  "\\$ $"  "qlogin")
;; (ee:password buf "password: $" "gw2") ; Here, gw2 is not password, but server-name.
;; (ee:command buf  "\\$ $"  "cd ~/gentoo")
;; (ee:command buf  "\\$ $"  "source ./startprefix")

;; 2. shorter form of above.
;;
;; (ee:persp)
;; (ee:init)
;; (ee:open-shell-buffer "*shell*(nig)")
;; (setq buf "*shell*(nig)") 
;;
;; (ee buf "\\$ $"
;;   '("ssh -X gw2.ddbj.nig.ac.jp"
;;     "qlogin"
;;     '(ee:password buf "password: $" "gw2")
;;     "cd ~/gentoo"
;;     "source ./startprefix"))


;; 3. sudo  ...
;; sudo not always but sometimes requires password.
;; In the following script, ee-c:commands waits for a set of prompt
;; to send corresponding command to the specified buffer.
;; If the first prompt (this case, "\\$ $") appeares,
;; the ee-c:commands loop is exited.
;; 
;; (ee:command buf "\\$ $" "sudo apt-get --force-yes -y install r-cran-*")
;; (ee-c:commands buf "sudo password input (if any)"
;; 				 '(
;; 				   '("\\$ $" "")
;; 				   '("password for [a-z]+:\\s+$" "azure" t)
;; 				   ))



;;; ----------------------------------------------------------------------------
;;;
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

(defun ee (buffer prompt job-list)
  (dolist (job job-list)
	(cond ((stringp job) (ee:command buffer prompt job))
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

(ee:read-password "~/.emacs.d/ee-pass.txt.gpg")





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

;;;
;;; TODO: integrate with a kind of perspective modes.
;;; http://rubikitch.com/2015/02/13/persp-mode/
;;;
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

