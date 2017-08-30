;;; emacs-expect.el --- Asynchronous automatic operation on shell buffers. -*- lexical-binding: t -*-

;; Copyright (C) 2016, 2017 Osamu Ogasawara

;; Author: O. Ogasawara <osamu.ogasawara@gmail.com>
;; Version: 1.11
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



(defconst emacs-expect-version "1.11.0")
(package-initialize)

;;; Commentary:

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
;;
;; (package-install 'cl-lib)
;; (package-install 'queue)
;; (package-install 'deferred)
;; (package-install 'dash)
;; (package-install 'subr-x)
;; (package-install 'pcre2el)


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
;; (require 'ee-persp)
;; (require 'emacs-expect)
;; (ee-inventory-load)
;; 
;; 2. Create a set of windows (perspective).
;;
;; (ee-persp) 

;; 3. Showing a shell buffer on a window.
;;
;; (ee-shell "*shell*(mac:1)")
;;

;; 4. Send a command to a shell buffer.
;;
;; (ee-run "*shell*(mac:1)" "\\$ $" "ls -F")

;; 5. Login to other hosts.
;;
;; (ee-load-catalog)
;;
;; ;; TIP: This function prints a list of user-name@host information loaded.
;; (ee-catalog-print) 
;;
;; (setq buf "*shell*(nig:1)")
;; (ee-shell buf) ;; opens up the shell buffer
;;
;; (ee-run buf  "\\$ $"  "ssh -X gw2.ddbj.nig.ac.jp")
;; (ee-run buf  "\\$ $"  "qlogin")
;; ;; Send a password to the buffer.
;; (ee-run buf "password: $" "your-account@gw2" 't) 
;; (ee-run buf  "\\$ $"  "cd ~/gentoo")
;; (ee-run buf  "\\$ $"  "./startprefix")


;; 6. Running a simple automaton:
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



;;; ----------------------------------------------------------------------

(defun ee-qelem-get-desc (qelem)
  (nth 0 qelem))


(defun ee-qelem-get-pred (qelem)
  (nth 1 qelem))


(defun ee-qelem-get-action (qelem)
  (nth 2 qelem))


(defun ee-qelem-get-tags (qelem)
  (nth 3 qelem))





;;; ----------------------------------------------------------------------

(defun ee-make-com (line)
	(concat "(ee-run buf \"\\\\\$ \$\" "  "\"" line "\")"))

(defun ee-null-line-p (line)
  (string= line ""))

(defun ee-nl-concat (l1 l2)
  (concat l1 "\n" l2))

(defun ee-expand ()
  (interactive)
  (let ((lines (split-string (buffer-substring-no-properties (region-beginning) (region-end)) "\n")))
	(insert (cl-reduce 'ee-nl-concat (cl-map 'list 'ee-make-com (cl-remove-if 'ee-null-line-p lines))))))



(defun ee-info-running-p ()
  (if ee-running-p "Running" "Stopped"))


(defun ee-info-keys-of-ee-queue ()
  (mapconcat 'identity (hash-table-keys ee-queue) ", "))


(defun ee-info-ee-queue ()
  (dolist (buffer (hash-table-keys ee-queue))
	(insert (format "\n  %s : %d" buffer (queue-length (gethash buffer ee-queue))))))


(defun ee-info ()
  (insert "\n")
  (insert (concat "Status : " (ee-info-running-p)))
  (insert (format "\nee-queue-total-length : %d" (ee-queue-total-length)))
  (insert "\nee-queue : ")
  (ee-info-ee-queue))



;;; ==============================
;;;
;;;   Job Queue
;;;
;;; ==============================


;;; ee-queue is a hash table which keeps information of
;;; buffer-name => a queue of (list desc pred action) lists.
(set 'ee-queue (make-hash-table :test #'equal))


(defun ee-queue-submit (buffer desc pred action tags)
  "This function submit a job to the job queue (ee-queue)."
  (if (not (gethash buffer ee-queue))
	  (puthash buffer (make-queue) ee-queue))

  (queue-enqueue (gethash buffer ee-queue) (list desc pred action tags)))


(defun ee-queue-empty (buffer)
  (queue-clear (gethash buffer ee-queue))
  (remhash buffer ee-queue))


(defun ee-queue-empty-all ()
  (clrhash ee-queue))


(defun ee-queue-dequeue (buffer)
  (queue-dequeue (gethash buffer ee-queue)))


(defun ee-queue-first (buffer)
  (queue-first (gethash buffer ee-queue)))


(defun ee-queue-prepend (buffer qelem)
  (queue-prepend (gethash buffer ee-queue) qelem))


(defun ee-queue-dequeue-until (buffer desc-rxt)
  (let ((qelem (ee-queue-dequeue buffer)))
	(while (and qelem
				(not (string-match
					  (rxt-pcre-to-elisp desc-rxt)
					  (ee-qelem-get-desc qelem))))
	  (setq qelem (ee-queue-dequeue buffer)))
	(if (not (null qelem))
		(ee-queue-prepend buffer qelem))))


(defun ee-queue-print-buffers ()
  (dolist (buf (hash-table-keys ee-queue))
	(insert "\n")
	(insert buf)))


(defun ee-queue-print-jobs (buffer)
  (let* ((elem-list (queue-all (gethash buffer ee-queue))))
	(dolist (elem elem-list)
	  (insert "\n")
	  (insert (car elem) ))))


(defun ee-queue-get-job-names (buffer)
  (let ((elem-list (queue-all (gethash buffer ee-queue)))
		(job-names '()))
	(dolist (elem elem-list)
	  (setq job-names (cons (ee-qelem-get-desc elem) job-names)))
	(setq job-names (reverse job-names))))



;;; This function is used in ee-start function
;;; to judge whether ee-queue is totally empty or not.
(defun ee-queue-total-length ()
  (-reduce '+ (-map 'ee-queue-length (hash-table-keys ee-queue))))
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

				  ;; if buffer does not exist, clear the buffer queue.
				  (ee-queue-empty buffer)
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
			  (if (= (ee-queue--total-length) 0)
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
  (ee-queue-empty-all))




;;; ========================================
;;;
;;;   Job submission utilities
;;;   (1) Simple submission.
;;;
;;; ========================================

(nth 3 '(a b))

(if (null (nth 3 '(a b))) "unhit" "hit")

;;; submit and start.

(defun ee-run (buffer prompt string &rest inst-desc-tags)
  (let ((inst (nth 0 inst-desc-tags))
		(desc (nth 1 inst-desc-tags))
		(tags (nth 2 inst-desc-tags)))
	(print inst)
	(cond ((null inst)
		   (ee-send-command buffer prompt string desc tags))
		  ((string= inst "c") ;; instruction is "command"
		   (ee-send-command buffer prompt string desc tags))
		  ((string= inst "p") ;; instruction is "password"
		   (ee-send-password buffer prompt string desc tags))
		  ((string= inst "l") ;; instruction is "elisp"
		   (ee-eval-elisp buffer prompt string desc tags))))
  (if (not ee-running-p)
	  (ee-start)))



(defun ee-send-command (buffer prompt command &optional desc tags)
  (progn
	(if (not desc) (setq desc command))
	(if (not tags) (setq tags '()))
	(ee-queue-submit
	 buffer
	 desc
	 (ee-pred-match-prompt buffer prompt)
	 (ee-action-send-command buffer command)
	 tags)))



(defun ee-send-password (buffer prompt account &optional desc tags)
  (progn
	(if (not desc) (setq desc "send an invisible string."))
	(if (not tags) (setq tags '()))
	(ee-queue-submit
	 buffer
	 desc
	 (ee-pred-match-prompt buffer prompt)
	 (ee-action-send-password buffer account)
	 tags)))



(defun ee-eval-elisp (buffer prompt elisp &optional desc tags)
  (progn
	(if (not desc) (setq desc "eval elisp."))
	(if (not tags) (setq tags '()))
	(ee-queue-submit
	 buffer
	 desc
	 (ee-pred-match-prompt buffer prompt)
	 (ee-action-eval-elisp buffer elisp)
	 tags)))



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
  (transition-table '()))


(defun ee-automaton-make-instance (t-table accept)
  (make-automaton
   :current-state 0
   :current-rule 0
   :accept-states accept
   :transition-table t-table))


(defun ee-automaton-append-rule (machine ss pred action ns)
  (setf (automaton-transition-table machine)
		(append (automaton-transition-table machine)
				(list (list ss pred action ns)))))


(defun ee-automaton-set-accept-states (machine accept-states)
  (setf (automaton-accept-states machine) accept-states))




;;;
;;;


(defun ee-automaton-pred (machine)
  (lambda ()
	  (ee-automaton-transite machine)
	  (ee-automaton-accept-p machine)))



(defun ee-automaton-transite (machine)
	(let* ((t-table (automaton-transition-table machine))
		   (row (automaton-current-rule machine))
		   (item (nth row t-table))
		   (state (nth 0 item))
		   (pred (nth 1 item))
		   (action (nth 2 item))
		   (next-state (nth 3 item)))
		  (if (= (automaton-current-state machine) state)
			  (if (funcall pred)
				  ;; pred is true.
				  (progn
					(funcall action)
					(setf (automaton-current-state machine) next-state)
					(setf (automaton-current-rule machine) 0))				
				;; pred is not true.
				(progn
				  (setf (automaton-current-rule machine) (+ row 1))
				  (if (>= (automaton-current-rule machine) (length t-table))
					  (setf (automaton-current-rule machine) 0)))
				))))
	  

(defun ee-automaton-accept-p (machine)
  (let* ((current-state (automaton-current-state machine))
		 (accept-states (automaton-accept-states machine)))	
		 (memq current-state accept-states)))



(defun ee-automaton-submit (buffer machine &optional desc tags)
  ;; initialize the machine before submit it to the ee-queue.
  (setf (automaton-current-rule machine) 0) 
  (ee-queue-submit buffer desc
				   (ee-automaton-pred machine)
				   (ee-action-true)
				   tags))



(defun ee-automaton-run (buffer machine &rest desc-tags)
  ;; initialize the machine before submit it to the ee-queue.
  (let ((desc (nth 0 desc-tags))
		(tags (nth 1 desc-tags)))
	(ee-automaton-submit buffer machine desc tags)
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



(defun ee-get-last-result (buffer &rest regex)
  (if (null regex) (setq regex "^\$ $"))
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

