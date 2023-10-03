(require 'go-ts-mode)

(defun iensu--go-mode-hook ()
  (add-hook 'before-save-hook #'lsp-format-buffer)
  (lsp-deferred))

(add-hook 'go-ts-mode-hook #'iensu--go-mode-hook)
