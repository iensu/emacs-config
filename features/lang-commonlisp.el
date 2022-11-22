;; Install SBCL and Quicklisp package manager (https://gist.github.com/jteneycke/7947353)
;; curl -O http://beta.quicklisp.org/quicklisp.lisp
;; sbcl --load quicklisp.lisp
;; (quicklisp-quickstart:install)
;; (ql:quickload "quicklisp-slime-helper")

;(load (expand-file-name "~/quicklisp/slime-helper.el"))

(use-package slime)

(defun iensu-lisp-hook ()
  (smartparens-strict-mode 1)
  (eldoc-mode 1))

(add-hook 'lisp-mode-hook (lambda ()
                            (setq inferior-lisp-program (executable-find "sbcl"))))
(add-hook 'lisp-mode-hook #'iensu-lisp-hook)
(add-hook 'slime-repl-mode-hook #'iensu-lisp-hook)
