(use-package go-mode
  :config
  (add-hook 'go-mode-hook #'lsp-deferred)
  (add-hook 'before-save-hook #'gofmt-before-save))
