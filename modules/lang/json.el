;;; lang/json.el --- JSON setup

(use-package json-mode
  :ensure t
  :config
  (iensu/add-auto-mode 'json-mode "\\.json$")
  (setq js-indent-level 2))
