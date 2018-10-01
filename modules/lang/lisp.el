;;; modules/lang/elisp.el --- Emacs Lisp -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defun iensu/-elisp-mode-hook ()
  (smartparens-mode -1)
  (paredit-mode 1)
  (rainbow-delimiters-mode 1))

(iensu-add-auto-mode 'emacs-lisp-mode "/Cask$")
(add-hook 'emacs-lisp-mode-hook 'iensu/-elisp-mode-hook)
(add-hook 'lisp-interaction-mode-hook 'iensu/-elisp-mode-hook)

(use-package slime-company)

(use-package slime
  :ensure t
  :init
  (require 'slime-autoloads)
  (setq inferior-lisp-program "/usr/local/bin/sbcl --noinform --no-linedit")
  (iensu-add-auto-mode 'common-lisp-mode ".sbclrc$" ".lisp$")
  :config
  (add-to-list 'slime-contribs 'slime-fancy)
  (add-to-list 'slime-contribs 'slime-company)
  (slime-setup slime-contribs)
  (add-hook 'common-lisp-mode-hook 'iensu/-elisp-mode-hook)
   (add-hook 'common-lisp-mode-hook
	   (lambda ()
	     (set (make-local-variable 'lisp-indent-function)
		  'common-lisp-indent-function)))
  (setq slime-lisp-implementations
        '((sbcl ("/usr/local/bin/sbcl" "--noinform" "--no-linedit" "--core" "sbcl.core-with-swank")))))
