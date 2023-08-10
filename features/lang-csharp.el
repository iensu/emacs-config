(use-package csharp-mode
  :mode ("\\.cs$" "\\.cshtml")
  :hook (csharp-mode . eglot-ensure)
  :config
  (defun iensu--csharp-mode-hook ()
    (c-set-offset 'arglist-intro '+)
    (setq c-basic-offset 4))
  (add-hook 'csharp-mode-hook #'iensu--csharp-mode-hook))

(add-to-list 'auto-mode-alist '("\\.csproj$" . xml-mode))
(add-to-list 'auto-mode-alist '("function.proj$" . xml-mode))
