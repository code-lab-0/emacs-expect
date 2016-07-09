;;; -*- lexical-binding: t -*-

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

;; -----

(provide 'ee-persp)
