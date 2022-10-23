(use-package csharp-mode
  :mode ("\\.cs$" "\\.cshtml")
  :hook (csharp-mode . eglot-ensure)
  :custom
  (flycheck-check-syntax-automatically '(save mode-enabled))
  :config
  (defun iensu--csharp-mode-hook ()
    (c-set-offset 'arglist-intro '+)
    (setq c-basic-offset 2))
  (add-hook 'csharp-mode-hook #'iensu--csharp-mode-hook))

(add-to-list 'auto-mode-alist '("\\.csproj$" . xml-mode))
(add-to-list 'auto-mode-alist '("function.proj$" . xml-mode))
