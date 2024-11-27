(require 'go-ts-mode)

(defun iensu--go-mode-hook ()
  (lsp-deferred)
  (flymake-mode 1))

(add-hook 'go-ts-mode-hook #'iensu--go-mode-hook)
