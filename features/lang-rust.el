(use-package rust-mode
  :bind (:map rust-mode-map
              ("C-c C-c" . rust-compile))
  :hook
  (rust-mode . eglot-ensure)
  (rust-mode . (lambda ()
                 (when (executable-find "cargo-clippy")
                   (add-to-list 'flycheck-enabled-checkers 'rust-clippy))))
  :config
  (setq rust-format-on-save t))
