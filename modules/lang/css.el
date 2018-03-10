;;; lang/css.el --- CSS setup

(use-package css-mode
  :config
  (setq css-indent-offset 2)
  (iensu-add-auto-mode 'css-mode "\\.styl$")
  (add-hook 'css-mode 'emmet-mode)
  (add-hook 'css-mode-hook (lambda ()
                             (rainbow-delimiters-mode)
                             (show-paren-mode))))
