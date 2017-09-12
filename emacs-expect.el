;;; emacs-expect.el --- Asynchronous automatic operation on shell buffers. -*- lexical-binding: t -*-

;; Copyright (C) 2016, 2017 Osamu Ogasawara

;; Author: O. Ogasawara <osamu.ogasawara@gmail.com>
;; Version: 0.9.0
;; Package-Requires: ((emacs "24.5) cl-lib queue deferred dash subr-x pcre2el)
;; Keywords:
;; URL: https://github.com/code-lab-0/emacs-expect

;; This file is not part of GNU Emacs.

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



(defconst emacs-expect-version "0.9.0")
(package-initialize)

;;; Commentary:

;; --------------------------------------------------------------------------
;; Installation
;;

;; 1. Prerequisites
;;
;; - GNU Emacs ver 24.5 or higher.
;;
;; Recommended software to be installed.
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
;;
;; (package-install 'cl-lib)
;; (package-install 'queue)
;; (package-install 'deferred)
;; (package-install 'dash)
;; (package-install 'subr-x)
;; (package-install 'pcre2el)


;; Common Lisp extensions for Emacs.
(unless (package-installed-p 'cl-lib)
  (package-install 'cl-lib))
(require 'cl-lib)


(require 'f)

;; Queue data structure
(unless (package-installed-p 'queue)
  (package-install 'queue))
(require 'queue)

;; Asynchronous task management 
;; https://github.com/kiwanami/emacs-deferred
(unless (package-installed-p 'deferred)
  (package-install 'deferred))
(require 'deferred) 

;; A modern list api for Emacs.
;; https://github.com/magnars/dash.el
(unless (package-installed-p 'dash)
  (package-install 'dash))
(require 'dash)

;; Extra lisp functions
;;(unless (package-installed-p 'subr-x)
;;  (package-install 'subr-x))
(require 'subr-x)

;; PCRE to Emacs regular expression conversion
;; https://github.com/joddie/pcre2el
(unless (package-installed-p 'pcre2el)
  (package-install 'pcre2el))
(require 'pcre2el)

;; elisp namespaces package.
(unless (package-installed-p 'namespaces)
  (package-install 'namespaces))
(require 'namespaces)

;;
;;(setq epg-gpg-program "gpg1")
;; 3. Prepareing an encrypted password file.
;;
;; You need to make a ~/.emacs.d/ee-catalog.txt file
;; which consists of tab-delimited name and password pairs.
;;
;; --------- ee-pass.txt ------------
;; realm-name1    password-of-server01
;; realm-name2    password-of-server02
;; ...
;; ----------------------------------
;; 
;; After creating this file, open this file with Emacs,
;; then encrypt it with M-x epa-encrypt-file.
;; https://www.gnu.org/software/emacs/manual/html_mono/epa.html
;;
;; 

;;; ----------------------------------------------------------------------------
;;; Usage

;; 1. Load the packages.
;; 
;; (require 'emacs-expect)
;;
;; 2. Job submission

(defmacro ee-send (queue buffer prompt command &optional conf)
  `(ee-submit-automaton
	,queue ,conf
	(ee-rule (eep-prompt ,buffer ,prompt)
			 (eea-insert ,buffer ,command))))


(defmacro ee-run-automaton (queue &optional conf &rest rules)
  `(progn
	 (ee-submit ,queue ,conf ,@rules)
	 (if (not ee-running-p)
		 (ee-start)
	   )
	 ))


(defun ee-submit-automaton (queue &optional conf &rest rules)
  (if (null conf) (setq conf (make-ee-conf)))
  (let* ((desc (ee-conf-desc conf))
		 (tags (ee-conf-tags conf))
		 (acc  (ee-conf-acc conf))
 		 (machine (ee-automaton-make-instance rules acc)))
 	  (ee-enqueue 
 	   queue
 	   (ee-automaton-pred queue machine)
 	   (ee-action-true)
 	   desc tags)))


(defstruct ee-conf
  (desc " --- ")
  (tags '())
  (acc '(1)))


(defun ee-shell (buf-name)
  (let* ((w (selected-window))
		 (bn (shell buf-name)))
	(select-window w)
	bn))

;;; ee-close-shell

;;;
;;;
;;;

(defun eep-prompt (buffer regex)
  (ee-pred-match-prompt buffer regex))

(defun eep-true ()
  (lambda () t))


(defun eea-insert (buffer string)
  (ee-action-send-command buffer string))


(defun eea-passwd (buffer account)
  (ee-action-send-password buffer account))

(defun eea-true ()
  (lambda () t))

(defun eea-ee-stop ()
  (lambda () (ee-stop)))

(defun eep-result-has (buffer regex)
  (ee-pred-match-result buffer regex))

;;;
;;;
;;;


(cl-defun ee-rule (pred action &key (tr '(0 1)) (desc "" ))
  (let ((cs (nth 0 tr))
		(ns (nth 1 tr)))			
	(list cs pred action ns desc)))



;;; ----------------------------------------------------------------------

(defun ee-qelem-get-desc (qelem)
  (nth 0 qelem))


(defun ee-qelem-get-pred (qelem)
  (nth 1 qelem))


(defun ee-qelem-get-action (qelem)
  (nth 2 qelem))


(defun ee-qelem-get-tags (qelem)
  (nth 3 qelem))




;;;-----------------------------------------


(defun ee-info-running-p ()
  (if ee-running-p "Running" "Stopped"))


(defun ee-info-keys-of-ee-bunch ()
  (mapconcat 'identity (hash-table-keys ee-bunch) ", "))


(defun ee-info-ee-bunch ()
  (dolist (buffer (hash-table-keys ee-bunch))
	(insert (format "\n  %s : %d" buffer (queue-length (gethash buffer ee-bunch))))))


(defun ee-info ()
  (insert "\n")
  (insert (concat "Status : " (ee-info-running-p)))
  (insert (format "\nee-bunch-total-length : %d" (ee-bunch-total-length)))
  (insert "\nee-bunch : ")
  (ee-info-ee-bunch))



;;; ==============================
;;;
;;;   The Bunch of Job Queues
;;;
;;; ==============================


;;; ee-bunch is a hash table which keeps information of
;;; buffer-name => a queue of (list desc pred action tags) lists.
(set 'ee-bunch (make-hash-table :test #'equal))



(defun ee-enqueue (queue pred action desc tags)
  "This function submit a job to the job queue (ee-bunch)."
  (if (not (gethash queue ee-bunch))
	  (puthash queue (make-queue) ee-bunch))
  (queue-enqueue (gethash queue ee-bunch) (list pred action desc tags)))



(defun ee-bunch-empty (queue)
  (queue-clear (gethash queue ee-bunch))
  (remhash queue ee-bunch))


(defun ee-bunch-empty-all ()
  (clrhash ee-bunch))


(defun ee-bunch-dequeue (queue)
  (queue-dequeue (gethash queue ee-bunch)))


(defun ee-bunch-first (queue)
  (queue-first (gethash queue ee-bunch)))


(defun ee-bunch-prepend (queue qelem)
  (queue-prepend (gethash queue ee-bunch) qelem))


(defun ee-bunch-dequeue-until (queue desc-rxt)
  (let ((qelem (ee-bunch-dequeue queue)))
	(while (and qelem
				(not (string-match
					  (rxt-pcre-to-elisp desc-rxt)
					  (ee-qelem-get-desc qelem))))
	  (setq qelem (ee-bunch-dequeue queue)))
	(if (not (null qelem))
		(ee-bunch-prepend queue qelem))))


(defun ee-bunch-print-queues ()
  (dolist (queue (hash-table-keys ee-bunch))
	(insert "\n")
	(insert queue)))


(defun ee-bunch-print-jobs (queue)
  (let* ((elem-list (queue-all (gethash queue ee-bunch))))
	(dolist (elem elem-list)
	  (insert "\n")
	  (insert (nth 2 elem)))))


(defun ee-bunch-get-job-names (queue)
  (let ((elem-list (queue-all (gethash queue ee-bunch)))
		(job-names '()))
	(dolist (elem elem-list)
	  (setq job-names (cons (ee-qelem-get-desc elem) job-names)))
	(setq job-names (reverse job-names))))



;;; This function is used in ee-start function
;;; to judge whether ee-bunch is totally empty or not.
(defun ee-bunch-total-length ()
  (-reduce '+ (-map 'ee-bunch-length (hash-table-keys ee-bunch))))
(defun ee-bunch-length (queue)
  (queue-length (gethash queue ee-bunch)))


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
			  
			  (dolist (queue (hash-table-keys ee-bunch))

				(let* ((q (gethash queue ee-bunch))
					   (qelem (if q (queue-first q) nil))
					   (pred (nth 0 qelem))
					   (action (nth 1 qelem))
					   (desc (nth 2 qelem))
					   (pred-result (if pred (funcall pred) nil)))

				 ;;(ee-log-debug queue desc)
				  
				  (if pred-result
					  (progn
						(ee-log-info queue (concat "(t) " desc))
						(funcall action)
						(queue-dequeue (gethash queue ee-bunch)))))

				  )))

		  (deferred:nextc it
			(lambda () 
			  (deferred:process "sh" "-c" "sleep 1")))
		  (deferred:nextc it
			(lambda () 
			  (if (= (ee-bunch-total-length) 0)
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
  (ee-bunch-empty-all))




;;; ========================================
;;;
;;;   Job submission utilities
;;;   (1) Simple submission.
;;;
;;; ========================================


;;; ========================================
;;; predicates and actions.
;;; ========================================

(defun ee-pred-true ()
  (lambda () t))



;; (defun ee-pred-match-prompt (buffer prompt)
;;   (lambda ()
;; 	(string-match
;; 	 (rxt-pcre-to-elisp prompt)
;; 	 (ee-buffer-tail-chars 100 buffer))))



(defun ee-pred-match-prompt (buffer prompt)
  (ee-pred-match-last buffer prompt 100))


(defun ee-pred-match-result (buffer regex)
  (lambda ()
	(string-match
	 (rxt-pcre-to-elisp regex)
	 (ee-get-last-result buffer))))




(defun ee-pred-match-last (buffer regex num-chars)
  (lambda ()
	(string-match
	 (rxt-pcre-to-elisp regex)
	 (ee-buffer-tail-chars num-chars buffer))))





(defun ee-pred-match-last-nth-line (buffer regex nth num-chars)
  (lambda ()
	(let ((line (last-nth-line (buffer nth num-chars)))
		  (string-match
		   (rxt-pcre-to-elisp regex) line)))))





(defun last-n-lines (buf n max-chars)
  (let ((lines (split-string (ee-buffer-tail-chars 500 buf) "\n")))
	(last lines n)))


(defun last-nth-line (buf n max-chars)
  (let ((lines (last-n-lines buf n max-chars)))
	(car lines)))



(defun ee-action-send-command (buffer command)
  (lambda ()
	(ee-buffer-send-input buffer command)))


(defun ee-action-eval-elisp (buffer func)
  (lambda ()
	(set-buffer buffer)
	(funcall func)))



(defun ee-action-send-password (buffer account)
  (lambda ()
	(select-window (active-minibuffer-window))
	(run-with-timer .2 nil 'insert (ee-get-password account))
	(run-with-timer .3 nil 'execute-kbd-macro (kbd "RET"))
	(set-buffer buffer)))


(defun ee-action-send-to-minibuffer (buffer string)
  (lambda ()
	(select-window (active-minibuffer-window))
	(run-with-timer .2 nil 'insert string)
	(run-with-timer .3 nil 'execute-kbd-macro (kbd "RET"))
	(set-buffer buffer)))


	;;(comint-send-string
	;; buffer
	;; (concat (ee-get-password account) "\n"))))


(defun ee-action-true ()
  (lambda () t))



;;; ========================================
;;;
;;;   Job submission utilities
;;;   (2) simple automaton
;;;
;;; ========================================

;; An example of the simple automaton:
;;
;; (ee-automaton-run "*shell*" "an example of the simple automaton"
;; 	  (ee-automaton-make-instance
;; 	   ;; transition table
;; 	   (list
;; 		(list 0 (ee-pred-match-prompt "*shell*" "\\$ $")
;; 			   (ee-action-send-command "*shell*" "date") 1)
;; 		(list 0 (ee-pred-match-prompt "*shell*" "% $")
;; 			  (ee-action-send-command "*shell*" "bash") 0)
;; 		(list 0 (ee-pred-match-prompt "*shell*" ">>> $")
;; 			  (ee-action-send-command "*shell*" "python") 0)
;; 		(list 0 (ee-pred-match-prompt "*shell*" "your name: $")
;; 			  (ee-action-send-command "*shell*" "You") 0))
;; 	   ;; accept states
;; 	   '(1)))


(defstruct automaton
  (current-state 0)
  (current-rule  0)
  (accept-states '())
  (error-status 0)
  (plist '())
  (transition-table '()))


(defun ee-automaton-make-instance (t-table accept)
  (make-automaton
   :current-state 0
   :current-rule 0
   :accept-states accept
   :transition-table t-table
   :error-status 0
   :plist '()
   ))


(defun ee-transition-table-append-rule (transition-table rule)
	  (append transition-table (list rule)))



(defun ee-automaton-append-rule (machine ss pred action ns)
  (setf (automaton-transition-table machine)
		(ee-transition-table-append-rule
		 (automaton-transition-table machine)
		 (list ss pred action ns))))


(defun ee-automaton-set-accept-states (machine accept-states)
  (setf (automaton-accept-states machine) accept-states))



;;;

(defun ee-date-time ()
   (concat
	(format-time-string "%Y-%m-%dT%T")
	((lambda (x) (concat (substring x 0 3) ":"
						 (substring x 3 5)))
	 (format-time-string "%z"))))


;;;
;;;


(defun ee-automaton-pred (queue machine)
  (lambda ()
	(if (< (automaton-error-status machine) 0) ;; if the machine ends with errors.
		nil
	  ;; else
	  (progn
		(ee-automaton-transite queue machine)
		(ee-automaton-accept-p machine)))))


(defvar *ee-log-level* "info")
(defvar *ee-log-file*  "/tmp/emacs-expect")
(setq *ee-log-file* "/tmp/emacs-expect")

(defun ee-log-info (queue desc)
  (f-append-text
   (concat (ee-date-time) "\t" (concat "INFO " desc "\n"))
   'utf-8
   (concat *ee-log-file* "." queue ".log")))



(defun ee-automaton-transite (queue machine)
	(let* ((t-table (automaton-transition-table machine))
		   (row (automaton-current-rule machine))
		   (rule (nth row t-table))
		   (state (nth 0 rule))
		   (pred (nth 1 rule))
		   (action (nth 2 rule))
		   (next-state (nth 3 rule))
		   (desc (if (>= (length rule) 5) (nth 4 rule) ""))
		   (matched nil))
		   
	  ;;(ee-log-debug queue desc)

	  (if (= (automaton-current-state machine) state) ;; if state matches
		  (if (funcall pred)
			  ;; pred is true.
			  (progn
				(ee-log-info queue (concat "(t) " desc))
				(funcall action)
				(setf (automaton-current-state machine) next-state))))
			
	  (setf (automaton-current-rule machine) (+ row 1))
	  (if (>= (automaton-current-rule machine) (length t-table))
		  (setf (automaton-current-rule machine) 0))
	  ))




(defun ee-automaton-accept-p (machine)
  (let* ((current-state (automaton-current-state machine))
		 (accept-states (automaton-accept-states machine))
		 (is-accepted (memq current-state accept-states))
		 (accepted-state (if is-accepted current-state nil)))
	(if is-accepted
		(setf (automaton-error-status machine) accepted-state))
	is-accepted))
		 



(defun ee-automaton-submit (queue machine &optional desc tags)
  ;; initialize the machine before submit it to the ee-bunch.
  (setf (automaton-current-rule machine) 0) 
  (ee-enqueue queue 
			  (ee-automaton-pred queue machine)
			  (ee-action-true)
			  desc
			  tags))



(defun ee-automaton-run (queue machine &rest desc-tags)
  ;; initialize the machine before submit it to the ee-bunch.
  (let ((desc (nth 0 desc-tags))
		(tags (nth 1 desc-tags)))
	(ee-automaton-submit queue machine desc tags)
	(if (not ee-running-p)
		(ee-start))))

;;; ==============================
;;;
;;;   Buffer read/write utilities.
;;;
;;; ==============================


(defun ee-buffer-tail-chars (num-chars buffer)
  (set-buffer buffer)
  (buffer-substring-no-properties 
   (max (- (point-max) num-chars) (point-min)) 
   (point-max) ))



(cl-defun ee-get-last-result (buffer &key regex)
  (if (null regex) (setq regex "^\\$ "))
  (let ((result
		 (progn
		   (set-buffer buffer)
		   (dotimes (i 2)
			 (previous-line))
		   (buffer-substring-no-properties
			(max (point-min) (re-search-backward (rxt-pcre-to-elisp regex)))
			(point-max)))))
	(goto-char (point-max))
	result))


(defun ee-search-in-last-result (buffer regex)
	(string-match (rxt-pcre-to-elisp regex) (ee-get-last-result buffer)))



(defun ee-buffer-send-input (buffer command)
  (set-buffer buffer)
  (goto-char (point-max))
  (insert command)
  (comint-send-input))


;;; ========================================
;;;   catalog
;;;   A map of identifier => password
;;; ========================================

(set 'ee-catalog (make-hash-table :test #'equal))

(defun ee-get-password (account)
  (gethash account ee-catalog))


(defun ee-set-password (account password)
  (pushhash account password ee-catalog))


(defun ee-list-accounts ()
  (dolist (account (hash-table-keys ee-catalog))
	(insert "\n")
	(insert account)))


(defun ee-account-list ()
	(hash-table-keys ee-catalog))



(defun ee-clear-accounts()
  (clrhash ee-catalog))


(defun ee-load-accounts (fname)
  (ee-catalog-read-file fname))
;;(defun ee-catalog-load ()
;;  (ee-catalog-read-file "~/.emacs.d/ee-catalog.txt"))


;;; ---

;;; http://ergoemacs.org/emacs/elisp_read_file_content.html
(defun ee-file-read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
	(insert-file-contents filePath)
	    (split-string (buffer-string) "\n" t)))


(defun ee-catalog-read-file (filePath)
  (dolist (line (ee-file-read-lines filePath))
	(let* ((kv (split-string line "\t" t))
		   (key (car kv))
		   (value (cadr kv)))
	  (puthash key value ee-catalog)))
  )





;;; ----------

(provide 'emacs-expect)

