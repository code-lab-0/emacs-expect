;;; -*- lexical-binding: t -*-

;;; Emacs 24 has optional lexical binding, 
;;; which can be enabled on a per-buffer basis.

;; -------------------------------------------------------------------------
;; Emacs expect -- Asynchronous automatic operation of Emacs Shell buffers. 

(defconst emacs-expect-version "1.10")

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
;;
;; Before you install these packages (by using package.el),
;; you need to add Marmalade and Melpa
;; archives to the search list as follows.
;;
;; (require 'package)
;; (add-to-list 'package-archives
;; 			 '("marmalade" . "http://marmalade-repo.org/packages/"))
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; (package-initialize)

;; Common Lisp extensions for Emacs.
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



;; 3. Prepareing an encrypted password file.
;;
;; You need to make a ~/.emacs.d/inventory.txt file
;; which consists of tab-delimited name and password pairs.
;;
;; --------- ee-pass.txt ------------
;; your-account@server01    password-of-server01
;; your-account@server02    password-of-server02
;; ...
;; ----------------------------------
;; 
;; After creating this file, open this file with Emacs,
;; then encrypt it with M-x epa-encrypt-file.
;; https://www.gnu.org/software/emacs/manual/html_mono/epa.html


;;; ----------------------------------------------------------------------------
;;; Usage

;; 1. Load the packages.
;; 
;; (require 'ee-persp)
;; (require 'emacs-expect)

;; 2. Create a set of windows (perspective).
;;
;; (ee-persp) 

;; 3. Showing a shell buffer on a window.
;;
;; (ee-shell "*shell*(mac:1)")
;;
;; ;; TIP: This function prints a list of shell buffers which have been opened.
;; (ee-shell-buffer-list) 
;;

;; 4. Send a command to a shell buffer.
;;
;; (ee-run "*shell*(mac:1)" "\\$ $" "ls -F")

;; 5. Login to other hosts.
;;
;; (ee-inventory-load)
;;
;; ;; TIP: This function prints a list of user-name@host information loaded.
;; (ee-inventory-print) 
;;
;; (setq buf "*shell*(nig:1)")
;; (ee-shell buf)
;;
;; (ee-run buf  "\\$ $"  "ssh -X gw2.ddbj.nig.ac.jp")
;; (ee-run buf  "\\$ $"  "qlogin")
;; ;; Send a password to the buffer.
;; (ee-run buf "password: $" "your-account@gw2" 't) 
;; (ee-run buf  "\\$ $"  "cd ~/gentoo")
;; (ee-run buf  "\\$ $"  "./startprefix")



(defun ee-info-running-p ()
  (if ee-running-p "Running" "Stopped"))


(defun ee-info-keys-of-ee-queue ()
  (mapconcat 'identity (hash-table-keys ee-queue) ", "))

(defun ee-info-ee-queue ()
  (dolist (buffer (hash-table-keys ee-queue))
	(insert (concat "\n  " buffer " : " (queue-length (gethash buffer ee-queue))))))

(defun ee-info ()
  (insert "\n")
  (insert (concat "Status : " (ee-info-running-p)))
  (insert (format "\nee-queue-total-length : %d" (ee-queue-total-length)))
  (insert (concat "\nee-queue : " (ee-info-ee-queue))))



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


;;; ee-queue is a hash table which keeps information of
;;; buffer-name => a queue of (list desc pred action) lists.
(set 'ee-queue (make-hash-table :test #'equal))


(defun ee-queue-submit (buffer desc pred action)
  "This function submit a job to the job queue (ee-queue)."
  (if (not (gethash buffer ee-queue))
	  (puthash buffer (make-queue) ee-queue))

  (queue-enqueue (gethash buffer ee-queue) (list desc pred action)))


(defun ee-queue-clear (buffer)
  (queue-clear (gethash buffer ee-queue))
  (remhash buffer ee-queue))


(defun ee-queue-clear-all ()
  (clrhash ee-queue))


(defun ee-queue-dequeue (buf)
  (queue-dequeue (gethash buffer ee-queue)))


(defun ee-queue-print-buffers ()
  (hash-table-keys ee-queue))


(defun ee-queue-print (buffer)
  (let* ((elem-list (queue-all (gethash buffer ee-queue))))
	(dolist (elem elem-list)
	  (insert "\n")
	  (insert (car elem) ))))



;;; This function is used in ee-start function
;;; to judge whether ee-queue is totally empty or not.
(defun ee-queue-total-length ()
  (let ((sum
		 (-reduce
		  '+
		  (-map 
		   (lambda (buf) 
			 (if (queue-empty (gethash buf ee-queue)) 0 1))
		   (hash-table-keys ee-queue)))))
	sum))


(defun ee-queue-length (buffer)
  (queue-length (gethash buffer ee-queue)))


;;; ==============================
;;;
;;;   Job Execution
;;;
;;; ==============================

;;; ee-running-p definition.
(if (not (boundp 'ee-running-p))
	(setq ee-running-p nil))


(defun ee-start ()
  (setq ee-running-p t)
  (deferred:$
	(deferred:next
	  (lambda (x) (princ "ee is started")))
	(deferred:nextc it
	  (deferred:lambda (x)
		(deferred:$
		  (deferred:next
			(lambda ()
			  
			  (dolist (buffer (hash-table-keys ee-queue))
				(if (get-buffer buffer)
					(let* ((q (gethash buffer ee-queue))
						   (qelem (if q (queue-first q) nil))
						   (desc (car qelem))
						   (pred (car (cdr qelem)))
						   (action (car (cddr qelem)))					   
						   (pred-result (if pred (funcall pred) nil)))

					  (if pred-result
						  (progn
							(funcall action)
							(queue-dequeue (gethash buffer ee-queue)))))

				  ;; if buffer does not exit, clear the buffer queue.
				  (ee-queue-clear buffer)
				  ))))

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
			  (if (= (ee-queue-total-length) 0)
				  (setq ee-running-p nil))))
		  (if ee-running-p
			  (deferred:nextc it self)
			(progn 
			  (princ "ee is stopped.")
			  nil))
		  )))))


(defun ee-stop ()
  (setq ee-running-p nil))


(defun ee-init ()
  (ee-stop)
  (ee-queue-clear-all))




;;; ========================================
;;;
;;;   Job submission utilities
;;;   (1) Simple submission.
;;;
;;; ========================================


;;; submit and start functions.

(defun ee-run (buffer prompt string &rest password-p)
  (if (not password-p)
	  (ee-send-command buffer prompt string)
	(ee-send-password buffer prompt string)))


(defun ee-send-command (buffer prompt command)
  (ee-queue-submit
   buffer
   command
   (ee-pred-match-prompt buffer prompt)
   (ee-action-send-command buffer command))
  (if (not ee-running-p)
	  (ee-start)))


(defun ee-send-password (buffer prompt account)
  (ee-queue-submit buffer
			 "********"
			 (ee-pred-match-prompt buffer prompt)
			 (ee-action-send-password buffer account))

  (if (not ee-running-p)
	  (ee-start)))



;;; ========================================
;;; predicates and actions.
;;; ========================================

(defun ee-pred-true ()
  (lambda () t))


(defun ee-pred-match-prompt (buffer prompt)
  (lambda ()
	(string-match
	 (rxt-pcre-to-elisp prompt)
	 (ee-buffer-tail-chars 100 buffer))))



(defun ee-action-send-command (buffer command)
  (lambda ()
	(ee-buffer-send-input buffer command)))


(defun ee-action-send-password (buffer inventory)
  (lambda ()
	(set-buffer buffer)
	(comint-send-string
	 buffer
	 (concat (ee-inventory-get-password inventory) "\n"))))



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

(setq ee-automaton-machine-example
	  '((0  (ee-pred-match-prompt buf "\\$ $")
			(ee-action-true) 1)
		(0  (ee-pred-match-prompt buf "(y/n) $")
			(ee-action-send-input buf "Y") 0)
		(0  (ee-pred-match-prompt buf "password- $")
			(ee-action-password buf "your-password") 0)))


(defun ee-automaton-make-instance (machine accept)
  (make-automaton :current-state 0 :accept-states accept :machine machine))



;;;
;;;
;;;

(defun ee-automaton-automaton-pred (machine)
  (ee-automaton-transite machine)
  (ee-automaton-accept-p machine))


(defun ee-automaton-transite (machine)
  (catch 'break
	(dolist (item machine)
	  (let* ((state (nth 0 item))
			 (pred (nth 1 item))
			 (action (nth 2 item))
			 (next-state (nth 3 item)))
		(if (= (automaton-current-state machine) state)
			(if (funcall pred)
				(progn
				  (funcall action)
				  (setf (automaton-current-state machine) next-state)
				  (throw 'break) ;; break
				  )))))))
	  

(defun ee-automaton-accept-p (machine)
  (let* ((current-state (automaton-current-state machine))
		 (accept-states (automaton-accept-states machine)))
	
		 (memq current-state accept-state)))
				  



;;; ==============================
;;;
;;;   Buffer read/write utilities.
;;;
;;; ==============================


(defun ee-buffer-tail-chars (num-chars buffer)
  (set-buffer buffer)
  (buffer-substring-no-properties 
   (max (- (point-max) 100) (point-min)) 
   (point-max) ))


(defun ee-buffer-send-input (buffer command)
  (set-buffer buffer)
  (insert command)
  (comint-send-input))


;;; ========================================
;;;   inventory
;;;   A map of username@host => password
;;; ========================================

(set 'ee-inventory (make-hash-table :test #'equal))

(defun ee-inventory-get-password (account)
  (gethash account ee-inventory))

(defun ee-inventory-set-password (account password)
  (pushhash account password ee-inventory))


;;; ---

;;; http://ergoemacs.org/emacs/elisp_read_file_content.html
(defun ee-file-read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
	(insert-file-contents filePath)
	    (split-string (buffer-string) "\n" t)))


(defun ee-inventory-read-file (filePath)
  (dolist (line (ee-file-read-lines filePath))
	(let* ((kv (split-string line "\t" t))
		   (key (car kv))
		   (value (cadr kv)))
	  (puthash key value ee-inventory)))
  )

(defun ee-inventory-clear()
  (clrhash ee-inventory))

(defun ee-inventory-load ()
  (ee-inventory-read-file "~/.emacs.d/inventory.txt.gpg"))


(defun ee-inventory-print ()
  (dolist (account (hash-table-keys ee-inventory))
	(insert "\n")
	(insert account)))

	
;;; ----------

(provide 'emacs-expect)

