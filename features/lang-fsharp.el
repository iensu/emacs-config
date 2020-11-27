(use-package fsharp-mode
  :defer t
  :hook
  (fsharp-mode . lsp)
  :mode ("\\.fs$" . fsharp-mode))

(add-to-list 'auto-mode-alist '("\\.fsproj$" . xml-mode))
