(defun iensu--rust-mode-hook ()
  (setq-local lsp-rust-server 'rust-analyzer)
  (setq-local lsp-rust-clippy-preference "on")
  (setq-local lsp-rust-all-features t))

(use-package rust-mode
  :bind (:map rust-mode-map
              ("C-c C-c" . rust-compile))
  :hook
  (rust-mode . lsp)
  (rust-mode . iensu--rust-mode-hook)
  :config
  (setq rust-format-on-save t))

(use-package flycheck-rust
  :after (rust-mode)
  :hook
  (flycheck-mode . flycheck-rust-setup))
