(use-package web-mode
  :mode ("\\.html$" "\\.hbs$" "\\.handlebars$" "\\.jsp$" "\\.eex$" "\\.vue$" "\\.php$")
  :hook
  (web-mode . emmet-mode)
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
                (prettier-js-mode 1))))
  (flycheck-add-mode 'javascript-eslint 'web-mode))

;;;; CSS
(use-package css-mode
  :hook
  (css-mode-hook . emmet-mode)
  (css-mode-hook . rainbow-delimiters-mode)
  :custom
  (css-indent-offset 2))

(use-package rainbow-mode :hook (css-mode))

(use-package scss-mode :mode ("\\.scss$" "\\.styl$"))

(use-package emmet-mode
  :config
  (add-hook 'emmet-mode-hook
            (lambda ()
              (when (or (string-suffix-p ".jsx" (buffer-name))
                        (string-suffix-p ".tsx" (buffer-name)))
                (setq emmet-expand-jsx-className? t)))))
