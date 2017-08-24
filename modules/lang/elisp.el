;;; modules/lang/elisp.el --- Emacs Lisp

;;; Code:

(defun iensu/-elisp-mode-hook ()
  (message "Setting up elisp mode")
  (smartparens-mode -1)
  (paredit-mode 1)
  (rainbow-delimiters-mode 1))

(add-hook 'emacs-lisp-mode-hook 'iensu/-elisp-mode-hook)
(add-hook 'lisp-interaction-mode-hook 'iensu/-elisp-mode-hook)
