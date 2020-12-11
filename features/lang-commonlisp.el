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

(use-package slime
  :ensure t)

(use-package slime-company
  :after (slime company)
  :config
  (add-to-list 'slime-contribs 'slime-company)
  (add-to-list 'company-backends #'company-slime)
  (setq slime-company-completion 'fuzzy
        slime-company-after-completion 'slime-company-just-one-space))

(add-hook 'slime-repl-mode-hook #'iensu-slime-hook)
