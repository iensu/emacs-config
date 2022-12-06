(use-package geiser
  :hook
  (geiser-repl-mode . smartparens-strict-mode)
  :config
  (geiser-autodoc-mode 1))

(use-package geiser-guile)

(defun iensu-scheme-hook ()
  (smartparens-strict-mode 1)
  (eldoc-mode 1))

(add-hook 'scheme-mode-hook 'iensu-scheme-hook)
