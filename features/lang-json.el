(require 'json-ts-mode)

(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))

(defun iensu/json-mode-hook ()
  (setq js-indent-level 2))

(add-hook 'json-ts-mode-hook #'iensu/json-mode-hook)
