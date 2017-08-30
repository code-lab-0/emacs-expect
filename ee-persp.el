;;; -*- lexical-binding: t -*-

;;; ==============================
;;;
;;;   Perspectives
;;;
;;; ==============================

;; PCRE to Emacs regular expression conversion
;; https://github.com/joddie/pcre2el
(require 'pcre2el)


(defun ee-persp ()
  (interactive)
  (progn
	(delete-other-windows)
	(let ((h (ceiling (* (window-height) 0.7)))
		  (w (ceiling (* (window-width) 0.6))))
	  (split-window 'nil w t)
	  (split-window 'nil h 'nil)
	  (other-window 3))

	(setq ee-window-list (window-list))
	
	(progn
	  (other-window 1)
	  (switch-to-buffer "*scratch*")
	  (other-window 1)
	  (shell)
	  (other-window 1)
	  )))



(defun ee-open-shell (buf-name)
  (progn
	(if (> (/ (window-height) (frame-height)) 0.5)
		(select-window (nth 2 (window-list)))
	  	(select-window (nth 1 (window-list))))
	(shell buf-name)))



(defun ee-save-shell-buffer (buffer outfile)
  (let ((cb (current-buffer)))
	 (switch-to-buffer buffer)
	 (kill-ring-save (point-min) (point-max))
	
	 (find-file outfile)
	 (goto-char (point-max))
	 (yank)
	 (save-buffer)
	 (kill-buffer)

	 (switch-to-buffer cb)
	 ))




;; (defun ee-save-shell-buffer (buf)
;;   (switch-to-buffer buf)
;;   (write-file buf))


;; (defun ee-shell-close (buf-name)
;;   (switch-to-buffer buf-name)
;;   (ee-run buf-name "\\$ $" "exit")
;;   (kill-buffer))
;; ee-run function puts "exit" command in the queue,
;; but here, a method that wait for completion of "exit" command is needed.
;; An ee-action function that call kill-buffer function !!


(defun ee-shell-buffer-list ()
  (dolist (buf (buffer-list))
	(if (string-match
		 (rxt-pcre-to-elisp "\\*shell\\*")
		 (buffer-name buf))
		(progn
		  (insert "\n")
		  (insert (buffer-name buf))))))


;; -----

(provide 'ee-persp)
