;;; modules/lang/typescript.el --- TypeScript setup

;;; Code:
(defun iensu/use-local-tslint ()
  "Try to use local tslint executable from node_modules."
  (interactive)
  (let ((tslint (iensu/node-find-local-executable "tslint")))
    (when (and tslint
               (file-executable-p tslint))
      (setq-local flycheck-typescript-tslint-executable tslint))))

(defun setup-tide-mode ()
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled)
        typescript-indent-level 2)
  (iensu/use-local-tslint)
  (flycheck-add-next-checker 'typescript-tide '(t . typescript-tslint) 'append)
  (eldoc-mode +1)
  (company-mode +1)
  (tide-hl-identifier-mode 1))

(use-package typescript-mode
  :delight
  (typescript-mode "TS" :major)
  :config
  (iensu-add-auto-mode 'typescript-mode "\\.ts$"))

(use-package tide
  :delight " 潮"
  :config
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook #'setup-tide-mode))
