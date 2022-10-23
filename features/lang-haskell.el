(use-package haskell-mode
  :mode ("\\.hs\\'" "\\.xmobarrc")
  :config
  (add-hook 'haskell-mode-hook 'eglot-ensure)
  (add-hook 'haskell-literate-mode-hook 'eglot-enusre))
