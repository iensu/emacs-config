(require 'go-ts-mode)

(defun iensu--go-mode-hook ()
  (lsp-deferred))

(add-hook 'go-ts-mode-hook #'iensu--go-mode-hook)
