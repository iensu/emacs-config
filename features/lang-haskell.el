(use-package haskell-mode
  :mode ("\\.hs\\'" "\\.xmobarrc")
  :config
  (add-hook 'haskell-mode-hook #'lsp)
  (add-hook 'haskell-literate-mode-hook #'lsp))

(use-package lsp-haskell)
