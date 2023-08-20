(require 'toml-ts-mode)

(defun iensu--toml-mode-hook ()
  (electric-pair-mode 1))

(add-hook 'toml-ts-mode-hook #'iensu--toml-mode-hook)
