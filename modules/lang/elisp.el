;;; modules/lang/elisp.el --- Emacs Lisp -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defun iensu/-elisp-mode-hook ()
  (smartparens-mode -1)
  (paredit-mode 1)
  (rainbow-delimiters-mode 1)
  (setq lisp-indent-function 'common-lisp-indent-function))

(iensu-add-auto-mode 'emacs-lisp-mode "/Cask$")
(add-hook 'emacs-lisp-mode-hook 'iensu/-elisp-mode-hook)
(add-hook 'lisp-interaction-mode-hook 'iensu/-elisp-mode-hook)
