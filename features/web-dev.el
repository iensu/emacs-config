(use-package web-mode
  :mode ("\\.html$"
         "\\.hbs$"
         "\\.handlebars$"
         "\\.jsp$"
         "\\.eex$"
         "\\.vue$"
         "\\.php$"
         "\\.ejs$"
         "\\.njk$"))

(defun iensu--web-mode-hook ()
  (when (seq-contains-p '("html" "php")
                        (file-name-extension (buffer-file-name)))

    (prettier-js-mode 1))
  (rainbow-mode 1)
  (emmet-mode 1))

(add-hook 'web-mode-hook #'iensu--web-mode-hook)

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
