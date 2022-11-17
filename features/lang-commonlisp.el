;; Install SBCL and Quicklisp package manager (https://gist.github.com/jteneycke/7947353)
;; curl -O http://beta.quicklisp.org/quicklisp.lisp
;; sbcl --load quicklisp.lisp
;; (quicklisp-quickstart:install)
;; (ql:quickload "quicklisp-slime-helper")

;(load (expand-file-name "~/quicklisp/slime-helper.el"))

(use-package slime)

(use-package slime-company
  :after (slime company)
  :config
  (add-to-list 'slime-contribs 'slime-company)
  (add-to-list 'company-backends #'company-slime)
  (setq slime-company-completion 'fuzzy
        slime-company-after-completion 'slime-company-just-one-space))

(defun iensu-lisp-hook ()
  (smartparens-strict-mode 1)
  (eldoc-mode 1))

(add-hook 'lisp-mode-hook (lambda ()
                            (setq inferior-lisp-program (executable-find "sbcl"))))
(add-hook 'lisp-mode-hook #'iensu-lisp-hook)
(add-hook 'slime-repl-mode-hook #'iensu-lisp-hook)
