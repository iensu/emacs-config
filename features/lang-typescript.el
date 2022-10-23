(use-package typescript-mode
  :mode ("\\.ts$" "\\.tsx$")
  :hook
  ((typescript-mode . add-node-modules-path)
   (typescript-mode . eglot-ensure)
   (typescript-mode . prettier-js-mode))
  :custom
  (flycheck-check-syntax-automatically '(save mode-enabled))
  (typescript-indent-level 2)
  :config
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (add-hook 'web-mode-hook
            (lambda ()
              (when (and buffer-file-name
                         (string-equal "tsx" (file-name-extension buffer-file-name)))
                (eglot-ensure)))))
