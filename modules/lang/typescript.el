;;; lang/typescript.el --- TypeScript setup

(defun setup-tide-mode ()
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled)
        typescript-indent-level 2)
  (eldoc-mode +1)
  (company-mode +1))

(use-package typescript-mode
  :defer t
  :ensure t
  :config
  (iensu/add-auto-mode 'typescript-mode "\\.ts$"))

(use-package tide
  :defer t
  :ensure t
  :config
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook #'setup-tide-mode))
