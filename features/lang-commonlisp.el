;; Install SBCL and Quicklisp package manager (https://gist.github.com/jteneycke/7947353)
;; curl -O http://beta.quicklisp.org/quicklisp.lisp
;; sbcl --load quicklisp.lisp
;; (quicklisp-quickstart:install)
;; (ql:quickload "quicklisp-slime-helper")

;(load (expand-file-name "~/quicklisp/slime-helper.el"))

(use-package sly
  :ensure t
  :vc (sly :url "https://github.com/joaotavora/sly"
           :rev "3ffa216d0818972f7a7fea38a566a6b570349f3b")
  :commands (sly)
  :config (setq inferior-lisp-program (executable-find "sbcl")))

(defun iensu-lisp-hook ()
  (smartparens-strict-mode 1)
  (eldoc-mode 1))

(add-hook 'sly-mode-hook #'iensu-lisp-hook)
(add-hook 'lisp-mode-hook #'iensu-lisp-hook)
(add-hook 'slime-repl-mode-hook #'iensu-lisp-hook)
