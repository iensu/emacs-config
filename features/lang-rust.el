(use-package rust-mode
  :bind (:map rust-mode-map
              ("C-c C-c" . rust-compile))
  :hook
  (rust-mode . eglot-ensure)
  :config
  (setq rust-format-on-save t))

(use-package flycheck-rust
  :after (rust-mode)
  :hook
  (flycheck-mode . flycheck-rust-setup))
