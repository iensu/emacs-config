;;; modules/lang/rust.el --- Rust setup

;;; Code:

(use-package rust-mode
  :bind (:map rust-mode-map
         ("C-c <tab>" . rust-format-buffer))
  :config
  (iensu-add-auto-mode 'rust-mode "\\.rs$"))

(use-package cargo
  :config
  (add-hook 'rust-mode-hook #'cargo-minor-mode))

(use-package flycheck-rust
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package racer
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'rust-mode-hook #'eldoc-mode)
  (setq racer-cmd "~/.cargo/bin/racer"
        racer-rust-src-path "/usr/local/src/rustc-1.8.0/src"))

(use-package company-racer
  :config
  (add-hook 'rust-mode-hook (lambda ()
                              (add-to-list 'company-backends 'company-racer)))
  (setq company-tooltip-align-annotations t
        company-racer-rust-src "/usr/local/src/rustc-1.8.0/src"))
