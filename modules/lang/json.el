;;; lang/json.el --- JSON setup

(use-package json-mode
  :config
  (iensu-add-auto-mode 'json-mode "\\.json$")
  (setq js-indent-level 2))
