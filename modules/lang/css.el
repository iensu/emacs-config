;;; lang/css.el --- CSS setup

(use-package rainbow-mode
  :config
  (add-hook 'css-mode-hook (lambda ()
                             (rainbow-mode))))

(defun iensu--setup-css ()
  (setq css-indent-offset 2)
  (emmet-mode 1)
  (rainbow-delimiters-mode 1)
  (show-paren-mode 1)
  (xah-css))

(use-package css-mode
  :bind (:map css-mode-map
              ("C-." . company-complete-common-or-cycle))
  :config
  (add-hook 'css-mode-hook 'iensu--setup-css))
