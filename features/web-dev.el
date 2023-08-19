(use-package web-mode
  :mode ("\\.html$" "\\.hbs$" "\\.handlebars$" "\\.jsp$" "\\.eex$" "\\.vue$" "\\.php$")
  :hook
  (web-mode . emmet-mode)
  (web-mode . flymake-eslint-enable)
  :custom
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-attr-indent-offset 2)
  (web-mode-attr-value-indent-offset 2)
  (web-mode-enable-css-colorization t)
  (web-mode-enable-current-element-highlight t)
  (web-mode-enable-current-column-highlight t)
  :config
  (add-hook 'web-mode-hook
            (lambda ()
              (yas-activate-extra-mode 'js-mode)
              (when (executable-find "prettier")
                (prettier-js-mode 1)))))

;;;; CSS
(use-package css-mode
  :config
  (add-hook 'css-mode-hook (lambda ()
                             (emmet-mode 1)
                             (prettier-js-mode 1))))

(use-package rainbow-mode :hook (css-mode))

(use-package scss-mode :mode ("\\.scss$" "\\.styl$"))

(use-package emmet-mode
  :config
  (add-hook 'emmet-mode-hook
            (lambda ()
              (when (or (string-suffix-p ".jsx" (buffer-name))
                        (string-suffix-p ".tsx" (buffer-name)))
                (setq emmet-expand-jsx-className? t)))))
