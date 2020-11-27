(use-package sql-mode
  :ensure nil
  :mode "\\.psql$"
  :config
  (add-hook 'sql-mode-hook
            (lambda ()
              (when (string= (file-name-extension buffer-file-name) "psql")
                (setq-local sql-product 'postgres)))))
