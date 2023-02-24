(use-package rust-mode
  :bind (:map rust-mode-map
              ("C-c C-c" . iensu/rust-check-project)
              ("C-c C-t" . rust-test))
  :hook
  (rust-mode . eglot-ensure)
  :config
  (setq rust-format-on-save t))

(defvar iensu-rust-check-project-clippy-params '()
  "Params to send to the clippy command.")

(defun iensu/rust-check-project ()
  "Run either Clippy or standard `cargo check' to check the current project."
  (interactive)
  (let* ((root-dir (project-root (project-current t)))
         (cargo-file (expand-file-name (concat root-dir "Cargo.toml"))))
    (if (not (executable-find "cargo-clippy"))
        (compile (format "cargo check --manifest-path=%s --workspace" cargo-file))
      (let ((clippy-args (if iensu-rust-check-project-clippy-params
                             (string-join (append '("--") iensu-rust-check-project-clippy-params) " ")
                           "")))
        (compile (format "cargo clippy --manifest-path=%s %s" cargo-file clippy-args))))))
