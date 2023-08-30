(require 'csharp-mode)

(dolist (ext '("\\.cs$" "\\.cshtml"))
  (add-to-list 'auto-mode-alist `(,ext . csharp-ts-mode)))

(add-to-list 'auto-mode-alist '("\\.csproj$" . xml-mode))
(add-to-list 'auto-mode-alist '("function.proj$" . xml-mode))

(defun iensu--csharp-mode-hook ()
  (c-set-offset 'arglist-intro '+)
  (setq c-basic-offset 4)
  (lsp-deferred))

(add-hook 'csharp-ts-mode-hook #'iensu--csharp-mode-hook)
