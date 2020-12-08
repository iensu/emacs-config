;; Install SBCL and Quicklisp package manager (https://gist.github.com/jteneycke/7947353)
;; curl -O http://beta.quicklisp.org/quicklisp.lisp
;; sbcl --load quicklisp.lisp
;; (quicklisp-quickstart:install)
;; (ql:quickload "quicklisp-slime-helper")

(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program
      (string-trim (shell-command-to-string "which sbcl")))

(defun iensu-slime-hook ()
  (smartparens-strict-mode 1)
  (eldoc-mode 1))

(use-package sly
  :ensure t)

(add-hook 'sly-mrepl-mode-hook #'iensu-slime-hook)
