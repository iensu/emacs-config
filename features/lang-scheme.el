(use-package geiser
  :config
  (add-hook 'geiser-repl-mode 'iensu-scheme-hook)
  (geiser-autodoc-mode 1))
(use-package geiser-chicken)
(use-package geiser-guile)

(defun iensu-scheme-hook ()
  (smartparens-strict-mode 1)
  (eldoc-mode 1))

(add-hook 'scheme-mode 'iensu-scheme-hook)
