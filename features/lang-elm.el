(use-package elm-mode
  :config
  (setq elm-tags-on-save t
        elm-sort-imports-on-save t
        elm-format-on-save t)
  (add-hook 'elm-mode-hook #'lsp))
