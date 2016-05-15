;;; -*- lexical-binding: t -*-

(require 'emacs-expect)


;;; docker

(defun ee:init-docker-mac (buffer)
  (ee:command buffer  "\\$ $"  eval "$(docker-machine env default)")
  )

(defun ee:docker-run (buffer command)
  (ee:command buffer  "\\$ $"  (concat "docker run" command))
  (ee:command buffer  "# $" "export PS1=\"\\u@\\H:\\w (\\D{%F} \\t)\\n\\\\$ \"")
  )

(defun ee:docker-attach (buffer command)
  (ee:command buffer  "\\$ $"  (concat "docker attach" command))
  (ee:command buffer  "# $" "export PS1=\"\\u@\\H:\\w (\\D{%F} \\t)\\n\\\\$ \"")
  )

(defun ee:change-prompt (buffer prev-prompt)
  (ee:command buffer  prev-prompt "export PS1=\"\\u@\\H:\\w (\\D{%F} \\t)\\n\\\\$ \""))



;;; log-in

(defun ee:login-gw2 (buffer)
  (ee:command buffer  "\\$ $"  "ssh -X gw2.ddbj.nig.ac.jp")
  (ee:command buffer  "\\$ $"  "qlogin")
  (ee:password buffer "password: $" "gw2")
  )


(defun ee:login-rgmxx (buffer xx)
  (ee:command buffer  "\\$ $"  (concat "ssh -X 133.39.114." xx))
  (ee:password buffer "password: $" "rgm")
)


(defun ee:login-rgm01 (buffer)
  (ee:login-rgmxx buffer "41")
)


(defun ee:login-rgm02 (buffer)
  (ee:login-rgmxx buffer "42")
)


(defun ee:login-rgm03 (buffer)
  (ee:login-rgmxx buffer "43")
)


(defun ee:login-rgm04 (buffer)
  (ee:login-rgmxx buffer "44")
)


(defun ee:login-rgm05 (buffer)
  (ee:login-rgmxx buffer "45")
)


(defun ee:login-ogalab (buffer)
  (ee:command buffer  "\\$ $"  "ssh -X -p 3843 root@www.ogalab.net")
  (ee:password buffer "password: $" "ogalab")
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
