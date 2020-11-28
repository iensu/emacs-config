(add-hook 'python-mode-hook #'lsp)

;;;; Org src block support
(add-to-list 'org-babel-load-languages '(python . t))
