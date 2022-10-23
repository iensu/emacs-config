(use-package fsharp-mode
  :defer t
  :hook
  (fsharp-mode . eglot-ensure)
  :mode ("\\.fs$" . fsharp-mode))

(add-to-list 'auto-mode-alist '("\\.fsproj$" . xml-mode))
