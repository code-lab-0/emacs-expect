;;; -*- lexical-binding: t -*-

(require 'emacs-expect)


(defun ee:login-gw2 (buffer)
  (ee:command buffer  "\\$ $"  "ssh gw2.ddbj.nig.ac.jp")
  (ee:command buffer  "\\$ $"  "qlogin")
  (ee:password buffer "password: $" "gw2")
  )


(defun ee:login-rgmxx (buffer)
  (ee:command buffer  "\\$ $"  "ssh 133.39.114.42")
  (ee:password buffer "password: $" "azure")
)

(defun ee:login-azure (buffer)
  (ee:command buffer  "\\$ $"  "ssh 192.168.1.7")
  (ee:password buffer "password: $" "azure")
)



(defun ee:apt-get (command)
  (ee:command "*shell*"  "\\$ $" command)
  (ee-c:commands "*shell*" "sudo password"
				 '(
				   '("\\$ $" "")
				   '("password for [a-z]+:\\s+$" "azure" t)
				   '("\\[Y\\/n\\]\\s+$" "Y")
				   ))
  )


;;; -----------

(provide 'emacs-expect-util)
