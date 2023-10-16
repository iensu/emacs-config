(use-package rust-mode
  :bind (:map rust-mode-map
              ("C-c C-c" . iensu/rust-check-project)
              ("C-c C-t" . rust-test))
  :hook
  (rust-mode . lsp-deferred)
  :config
  (setopt rust-format-on-save t)
  (setopt lsp-rust-clippy-preference "on")
  (setopt lsp-rust-analyzer-lens-enable t)
  (setopt lsp-rust-analyzer-closing-brace-hints t)
  (setopt lsp-rust-analyzer-display-chaining-hints t)
  (setopt lsp-rust-analyzer-display-reborrow-hints "always")
  (setopt lsp-rust-analyzer-lens-references-method-enable t)
  (setopt lsp-rust-analyzer-display-closure-return-type-hints t))

(defvar iensu-rust-check-project-clippy-params '()
  "Params to send to the clippy command.")

(defun iensu/rust-check-project ()
  "Run either Clippy or standard `cargo check' to check the current project."
  (interactive)
  (let* ((root-dir (project-root (project-current t)))
         (default-directory root-dir))
    (if (not (executable-find "cargo-clippy"))
        (compile (format "cargo check --workspace"))
      (let ((clippy-args (if iensu-rust-check-project-clippy-params
                             (string-join (append '("--") iensu-rust-check-project-clippy-params) " ")
                           "")))
        (compile (format "cargo clippy %s" clippy-args))))))
