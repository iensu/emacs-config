(use-package typescript-mode
  :mode ("\\.ts$" "\\.tsx$")
  :config
  (defun iensu/typescript-mode-hook ()
    (setq typescript-indent-level 2)
    (add-node-modules-path)
    (rainbow-mode 1)
    (prettier-js-mode 1)
    (when (and buffer-file-name
               (string-equal "tsx" (file-name-extension buffer-file-name)))
      (tsx-ts-mode))
    (eglot-ensure))
  (add-hook 'typescript-mode-hook #'iensu/typescript-mode-hook))
