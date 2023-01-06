(use-package rust-mode
  :bind (:map rust-mode-map
              ("C-c C-c" . rust-run-clippy)
              ("C-c C-t" . rust-test))
  :hook
  (rust-mode . eglot-ensure)
  :config
  (setq rust-format-on-save t))
