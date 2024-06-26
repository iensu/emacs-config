(use-package sql-mode
  :mode "\\.psql$"
  :ensure nil
  :config
  (add-hook 'sql-mode-hook
            (lambda ()
              (when (string= (file-name-extension buffer-file-name) "psql")
                (setopt sql-product 'postgres)))))
