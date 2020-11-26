(setq flycheck-disabled-checkers (append flycheck-disabled-checkers '(javascript-jshint)))

(setq js-switch-indent-offset 2)
(define-key js-mode-map (kbd "M-.") nil)

(use-package js2-mode
  :mode ("\\.js\\'")
  :interpreter ("node" "nodejs")
  :custom
  (js2-indent-level 2)
  (js2-highlight-level 3)
  :hook
  (js2-mode . electric-indent-mode)
  (js2-mode . rainbow-delimiters-mode)
  (js2-mode . smartparens-mode)
  (js2-mode . lsp)
  (js2-mode . prettier-js-mode)
  :config
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)
  (js2-mode-hide-warnings-and-errors)
  (flycheck-add-mode 'javascript-eslint 'js2-mode))

(use-package rjsx-mode
  :mode ("\\.jsx\\'")
  :hook
  (rjsx-mode . electric-indent-mode)
  (rjsx-mode . rainbow-delimiters-mode)
  (rjsx-mode . smartparens-mode)
  (rjsx-mode . emmet-mode)
  (rjsx-mode . lsp)
  (rjsx-mode . prettier-js-mode)
  :init
  (add-to-list 'magic-mode-alist
               '((lambda () (and buffer-file-name
                            (string-equal "js" (file-name-extension buffer-file-name))
                            (string-match "^import .* from [\"']react[\"']" (buffer-string))))
                 . rjsx-mode))
  :config
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
  (add-hook 'rjsx-mode-hook (lambda () (setq emmet-expand-jsx-className? t))))

(use-package js2-refactor
  :hook
  (rjsx-mode . js2-refactor-mode)
  (js2-mode . js2-refactor-mode))

(use-package xref-js2)
(use-package nvm)
(use-package add-node-modules-path
  :config
  (eval-after-load 'js2-mode
    '(add-hook 'js-mode-hook #'add-node-modules-path))
  (eval-after-load 'rjsx-mode
    '(add-hook 'js-mode-hook #'add-node-modules-path))
  (eval-after-load 'typescript-mode
    '(add-hook 'js-mode-hook #'add-node-modules-path)))
