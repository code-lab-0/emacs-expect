;;; -*- lexical-binding: t -*-

;;; Emacs 24 has optional lexical binding, 
;;; which can be enabled on a per-buffer basis.

;; -------------------------------------------------------------------------
;; Emacs expect -- Asynchronous automatic operation of Emacs Shell buffers. 

(defconst emacs-expect-version "1.01")

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

(require 'cl-lib)

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
					   (elem (if q (queue-first q) nil))
					   (desc (car qelem))
					   (pred (car (cdr qelem)))
					   (action (car (cddr qelem)))					   
					   (pred-result (if pred (funcall pred) nil)))

				  (if pred-result
					  (progn
						(funcall action)
						(queue-dequeue (gethash buffer ee:queue))))))))


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


(defun ee:initialize()
  (ee:load-pass)
  (ee:init))


(defun ee:init ()
  (ee:stop)
  (ee:clear-queue))


;; (defun ee:eval-qelem (qelem)
;;   (let* ((desc (car qelem))
;; 		(pred (car (cdr qelem)))
;; 		(action (car (cddr qelem)))
;; 		(result (if pred (funcall pred) nil)))

;; 	(if result (funcall action))
;; 	result))



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
;;; 1. (state pred action next-state) triads list.
;;; 2. current state
;;; 3. accept-states

;;; Usage of defstruct in Emacs-Lisp.
;;; https://curiousprogrammer.wordpress.com/2010/07/19/emacs-defstruct-vs-other-languages/
;; (defstruct person age name)
;; (defvar dave (make-person))
;; (setf (person-age dave) 20) ;; getter specifies the type!
;; (setf (person-name dave) "David Jones")
;; (message (person-name dave)) ;; -- David Jones

(defstruct automaton
  (current-state 0)
  (accept-states '())
  (machine '()))


;;; An example of the automaton

(setq ee:automaton:machine-example
	  '((0  (ee:pred:prompt buf "\\$ $")
			(ee:action:trivial) 1)
		(0  (ee:pred:prompt buf "(y/n) $")
			(ee:action:send-input buf "Y") 0)
		(0  (ee:pred:prompt buf "password: $")
			(ee:action:password buf "your-password") 0)))


;; (defun ee:automaton:instanciate (machine accept)
;;   (let* ((automaton make-automaton))

(defun ee:automaton:make-instance (machine accept)
  (let* ((automaton (make-automaton))
		 ((automatnon-accept-states automaton) accept)
		 ((automaton-machine automaton) machine))
	automaton))
					

(defun ee:automaton:print (automaton)
  (list 



;;; The symbol "ee-c:automaton-state" refers to
;;; a hash table of buffer-name => state correspondance,
;;; where the states are integers.
(set 'ee:automaton:automaton-current-state (make-hash-table :test #'equal))
(set 'ee:automaton:automaton-accept-state (make-hash-table :test #'equal))

;;; An automaton is a list of (predicate  action) pairs.
;;; Here, predicate and function are closures.

;;; The following three closure functions are
;;; the most fundamental.
;;; (ee:pred:prompt buf str)

(defun ee:action:send-input (buffer str)
  (lambda ()
	(ee:send-input buffer str))) 


(defun ee:action:password (buffer password)
  (lambda ()
	(set-buffer buffer)
	(comint-send-string buffer (concat password "\n"))))

  


(defun ee:automaton:automaton-pred (buffer machine)
  (ee:automaton:transite buffer machine)
  (ee:automaton:accept-p buffer))


(defun ee:automaton:transite (buffer machine)
  (catch 'break
	(dolist (item machine)
	  (let* ((state (nth 0 item))
			 (pred (nth 1 item))
			 (action (nth 2 item))
			 (next-state (nth 3 item)))
		(if (= (ee:automaton:current-state buf) state)
			(if (funcall pred)
				(progn
				  (funcall action)
				  (ee:automaton:set-current-state buf next-state)
				  (throw 'break) ;; break
				  )))))))
	  

(defun ee:automaton:accept-p (buffer)
  (let* ((current-state (gethash buffer ee:automaton:automaton-current-state))
		 (accept-states (gethash buffer ee:automaton:automaton-accept-state)))
	
		 (memq current-state accept-state)))
				  



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

(defun ee:set-password (name pass)
  (pushhash name pass ee:password))


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


(defun ee:load-password ()
  (ee:read-password "~/.emacs.d/ee-pass.txt.gpg"))

(ee:load-password)





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
	
;;; ----------

(provide 'emacs-expect)

