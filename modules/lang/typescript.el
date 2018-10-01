;;; modules/lang/typescript.el --- TypeScript setup

;;; Code:
(defun iensu/use-local-tslint ()
  "Try to use local tslint executable from node_modules."
  (interactive)
  (let ((tslint (iensu/node-find-local-executable "tslint")))
    (when (and tslint
               (file-executable-p tslint))
      (setq-local flycheck-typescript-tslint-executable tslint))))

(defun iensu/use-prettier ()
  (and (file-exists-p (expand-file-name ".prettierrc" (iensu/node-project-root)))
       (executable-find "prettier")))

;; (defun iensu/setup-tide-mode ()
;;   (interactive)
;;   (tide-setup)
;;   (flycheck-mode +1)
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled)
;;         typescript-indent-level 2)
;; ;  (iensu/use-local-tslint)
;;   (flycheck-add-next-checker 'typescript-tide '(t . typescript-tslint) 'append)
;;   (eldoc-mode +1)
;;   (company-mode +1)
;; ;  (tide-hl-identifier-mode nil) ; currently results in a error message if enabled
;;   )

(defun iensu/setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled)
        typescript-indent-level 2)
  (eldoc-mode +1)
  (company-mode +1))

(use-package typescript-mode
  :delight
  (typescript-mode "TS" :major)
  :config
  (iensu-add-auto-mode 'typescript-mode "\\.ts$")
  (add-hook 'typescript-mode-hook (lambda () (smartparens-strict-mode 1))))

(use-package npm-test
  :load-path (lambda () (iensu--config-file "packages")))

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
              ("C-c l r" . tide-refactor)
              ("C-c t" . npm-test-run-tests))
  :init
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  :after (typescript-mode company flycheck web-mode)
  :hook ((typescript-mode . iensu/setup-tide-mode)
         (typescript-mode . tide-hl-identifier-mode)
         (web-mode . (lambda ()
                       (when (and buffer-file-name
                                  (string-equal "tsx" (file-name-extension buffer-file-name)))
                         (iensu/setup-tide-mode)))))
  :config (if (not (iensu/use-prettier))
              (add-hook 'before-save-hook 'tide-format-before-save)))

(use-package add-node-modules-path
  :load-path (lambda () (iensu--config-file "packages")))

(use-package prettier-js
  :load-path (lambda () (iensu--config-file "packages"))
  :requires add-node-modules-path
  :config
  (cl-flet ((maybe-use-prettier ()
                                (add-node-modules-path)
                                (when (iensu/use-prettier)
                                  (prettier-js-mode 1))))
    (iensu/add-hooks #'maybe-use-prettier
                     'web-mode 'typescript-mode 'js2-mode)))
