;;; lang/javascript.el --- JavaScript and Web setup

;;; Code:

(defun iensu/pick-nodejs-version ()
  (let ((most-recent (caar (last (nvm--installed-versions))))
        (nvmrc? (lambda () (file-exists-p (concat (projectile-project-root) ".nvmrc")))))
    (cond ((not (projectile-project-p)) (nvm-use most-recent))
          ((not (funcall nvmrc?)) (nvm-use most-recent))
          (t (nvm-use-for (projectile-project-root))))))

(defun iensu/tern-restart-server ()
  "Restart the tern server."
  (interactive)
  (delete-process "Tern"))

(defun iensu/use-local-flow ()
  "Try to use local flow executable from node_modules."
  (interactive)
  (let ((flow (iensu/node-find-local-executable "flow")))
    (when (and flow
               (file-executable-p flow))
      (setq-local flycheck-javascript-flow-executable flow))))

(defun iensu/use-local-eslint ()
  "Try to use local eslint executable from node_modules."
  (interactive)
  (let ((global-eslint (executable-find "eslint"))
        (local-eslint (iensu/node-find-local-executable "eslint")))
    (setq-local flycheck-javascript-eslint-executable
                (if (file-executable-p local-eslint) local-eslint global-eslint))))

(defun iensu/counsel-nvm-use ()
  "Forward to `nvm-use'."
  (interactive)
  (ivy-read "nvm use "
            (let (cands)
              (mapcar
               (lambda (x) (push (car x) cands))
               (reverse (nvm--installed-versions)))
              (push "default" cands))
            :require-match t
            :sort t
            :action (lambda (x)
                      (nvm-use x))
            :caller 'iensu/counsel-nvm-use))

(defun iensu/-setup-javascript ()
  (electric-indent-mode t)
  (rainbow-delimiters-mode 1)
  (smartparens-mode 1)
  (js2-mode-hide-warnings-and-errors)
  (js2-imenu-extras-mode)
  (js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-m")
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)
  (iensu/pick-nodejs-version)
  (iensu/use-local-eslint)
  (iensu/use-local-flow)
  (when (executable-find "tern")
    (add-to-list 'company-backends 'company-tern)
    (tern-mode t))
  (setq js-switch-indent-offset 2
        js2-basic-offset 2
        js2-highlight-level 3)
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers '(javascript-jshint)))
  (define-key js-mode-map (kbd "M-.") nil))

(use-package js2-mode)

(use-package js2-jsx-mode
  :mode ("\\.js$" "\\.jsx$")
  :bind (:map js2-mode-map
              (("C-k" . js2r-kill)
               ("C-c t" . npm-test-run-tests)))
  :config
  (add-hook 'js2-mode-hook 'iensu/-setup-javascript)
  (add-hook 'js2-jsx-mode (lambda ()
                            (emmet-mode)
                            (setq emmet-expand-jsx-className? t)))
  (flycheck-add-mode 'javascript-eslint 'js2-jsx-mode)
  (flycheck-add-mode 'javascript-flow 'js2-jsx-mode)
  (flycheck-add-next-checker 'javascript-eslint 'javascript-flow))

(use-package js2-refactor
  :delight js2-refactor-mode)

(use-package flycheck-flow)

(use-package rjsx-mode
  :config
  (iensu-add-auto-mode 'rjsx-mode "\\.jsx?$")
  (add-hook 'rjsx-mode-hook 'iensu/-setup-javascript)
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
  (flycheck-add-mode 'javascript-flow 'rjsx-mode)
  (flycheck-add-next-checker 'javascript-eslint 'javascript-flow))

(use-package xref-js2
  :defer nil)

(use-package company-tern)

(use-package mocha)

(use-package nvm)

(use-package tern
  :delight " 鰺刺"
  :config
  (define-key tern-mode-keymap (kbd "M-.") nil)
  (define-key tern-mode-keymap (kbd "M-,") nil)
  (iensu-add-auto-mode 'json-mode "\\.tern-project$"))

(use-package web-mode
  :init
  (iensu-add-auto-mode 'web-mode "\\.html$" "\\.hbs$" "\\.handlebars$" "\\.jsp$" "\\.eex$")
  :config
  (setq web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-markup-indent-offset 2
        web-mode-attr-indent-offset 2
        web-mode-attr-value-indent-offset 2
        web-mode-enable-css-colorization t
        web-mode-enable-current-element-highlight t
        web-mode-enable-current-column-highlight t)
  (add-hook 'web-mode-hook #'(lambda () (yas-activate-extra-mode 'js-mode)))
  (add-hook 'web-mode-hook 'iensu/pick-nodejs-version)
  (add-hook 'web-mode-hook 'iensu/use-local-eslint)
  (add-hook 'web-mode-hook
            (lambda ()
              (when
                (iensu/setup-tide-mode))))
  (setq-default flychqeck-disabled-checkers
                (append flycheck-disabled-checkers '(javascript-jshint)))
  (add-hook 'web-mode-hook 'emmet-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode))

(use-package emmet-mode
  :config
  (add-hook 'emmet-mode-hook (lambda ()
                               (when (or (string-suffix-p ".jsx" (buffer-name))
                                         (string-suffix-p ".tsx" (buffer-name)))
                                 (setq emmet-expand-jsx-className? t)))))

;; Live updates as you type using `simple-httpd' and `impatient-mode'
;; 1. Go to index.html
;; 2. M-x `httpd-start'
;; 3. M-x `impatient-mode'
;; 4. M-x `imp-visit-buffer'
;;
;; Enable `impatient-mode' in any referenced buffer to enable live reloading
;; of updates there as well
(use-package simple-httpd)
(use-package impatient-mode)
