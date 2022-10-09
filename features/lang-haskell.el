(use-package haskell-mode)

(use-package lsp-haskell)

(use-package treemacs-treelib)

(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)
