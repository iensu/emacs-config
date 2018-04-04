;;; modules/lang/rust.el --- Rust setup

;;; Code:

(use-package rust-mode
  :config
  (iensu-add-auto-mode 'rust-mode "\\.rs$")
  (setq rust-format-on-save t))

(use-package cargo
  :config
  (add-hook 'rust-mode-hook #'cargo-minor-mode))

(use-package flycheck-rust
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package racer
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'rust-mode-hook #'eldoc-mode))
