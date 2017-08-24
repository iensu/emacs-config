;;; lang/rust.el --- Rust setup

(use-package rust-mode
  :defer t
  :ensure t
  :bind (:map rust-mode-map
         ("C-c <tab>" . rust-format-buffer))
  :config
  (iensu/add-auto-mode 'rust-mode "\\.rs$"))

(use-package cargo
  :defer t
  :ensure t
  :config
  (add-hook 'rust-mode-hook #'cargo-minor-mode))

(use-package flycheck-rust
  :defer t
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package racer
  :defer t
  :ensure t
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'rust-mode-hook #'eldoc-mode)
  (setq racer-cmd "~/.cargo/bin/racer"
        racer-rust-src-path "/usr/local/src/rustc-1.8.0/src"))

(use-package company-racer
  :defer t
  :ensure t
  :config
  (add-hook 'rust-mode-hook (lambda ()
                              (add-to-list 'company-backends 'company-racer)))
  (setq company-tooltip-align-annotations t
        company-racer-rust-src "/usr/local/src/rustc-1.8.0/src"))
