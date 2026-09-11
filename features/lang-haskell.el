(use-package haskell-mode
  :ensure t
  :vc (haskell-mode :url "https://github.com/haskell/haskell-mode"
                    :rev "4bdd38c22d8a54d3284b517e889863a1d3971998")
  :mode ("\\.hs\\'" "\\.xmobarrc")
  :config
  (add-hook 'haskell-mode-hook 'eglot-ensure)
  (add-hook 'haskell-literate-mode-hook 'eglot-enusre))
