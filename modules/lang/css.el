;;; lang/css.el --- CSS setup

(use-package rainbow-mode
  :config
  (add-hook 'css-mode-hook (lambda ()
                             (rainbow-mode))))

(use-package css-mode
  :config
  (setq css-indent-offset 2)
  (iensu-add-auto-mode 'css-mode "\\.styl$")
  (add-hook 'css-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook (lambda ()
                             (rainbow-delimiters-mode)
                             (show-paren-mode))))
