;;; modules/lang/typescript.el --- TypeScript setup

;;; Code:
(defun iensu/use-local-tslint ()
  "Try to use local tslint executable from node_modules."
  (interactive)
  (let ((tslint (iensu/node-find-local-executable "tslint")))
    (when (and tslint
               (file-executable-p tslint))
      (setq-local flycheck-typescript-tslint-executable tslint))))

(defun iensu/setup-tide-mode ()
  (interactive)
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
  (iensu-add-auto-mode 'typescript-mode "\\.ts$")
  (add-hook 'typescript-mode-hook (lambda () (smartparens-strict-mode 1))))

(use-package tide
  :delight " 潮"
  :bind (:map tide-mode-map
              ("C-." . company-files)
              ("M-." . tide-jump-to-definition)
              ("M-," . tide-jump-back)
              ("C-c l d" . tide-documentation-at-point)
              ("C-c l l" . tide-references)
              ("C-c l e" . tide-project-errors)
              ("C-c l f" . tide-fix)
              ("C-c l n" . tide-rename-symbol)
              ("C-c l r" . tide-refactor))
  :config
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook #'iensu/setup-tide-mode))
