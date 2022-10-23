(use-package go-mode
  :config
  (add-hook 'go-mode-hook 'eglot-ensure)
  (add-hook 'before-save-hook #'gofmt-before-save))
