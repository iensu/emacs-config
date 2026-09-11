;; -*- lexical-binding: t; -*-

(use-package web-mode
  :ensure t
  :vc (web-mode :url "https://github.com/fxbois/web-mode"
                :rev "ce24723eb900c455b488d224910519bd36af580a")
  :mode ("\\.html$"
         "\\.hbs$"
         "\\.handlebars$"
         "\\.jsp$"
         "\\.eex$"
         "\\.vue$"
         "\\.php$"
         "\\.ejs$"
         "\\.njk$"))

;; For HTML + CSS LSP support, install `vscode-langservers-extracted'
(defun iensu--web-mode-hook ()
  (when (seq-contains-p '("html" "php")
                        (file-name-extension (buffer-file-name)))

    (prettier-js-mode 1))
  (rainbow-mode 1)
  (emmet-mode 1)
  (lsp-deferred))

(add-hook 'web-mode-hook #'iensu--web-mode-hook)

;;;; CSS
(use-package css-mode
  :config
  (add-hook 'css-mode-hook (lambda ()
                             (emmet-mode 1)
                             (prettier-js-mode 1)
                             (lsp-deferred))))

(use-package rainbow-mode
  :ensure t
  :vc (rainbow-mode :url "https://github.com/emacsmirror/rainbow-mode"
                    :rev "f7db3b5919f70420a91eb199f8663468de3033f3")
  :hook (css-mode))

(use-package scss-mode
  :ensure t
  :vc (scss-mode :url "https://github.com/antonj/scss-mode"
                 :rev "cf58dbec5394280503eb5502938f3b5445d1b53d")
  :mode ("\\.scss$" "\\.styl$"))

(use-package emmet-mode
  :ensure t
  :vc (emmet-mode :url "https://github.com/smihica/emmet-mode"
                  :rev "322d3bb112fced57d63b44863357f7a0b7eee1e3")
  :config
  (add-hook 'emmet-mode-hook
            (lambda ()
              (when (or (string-suffix-p ".jsx" (buffer-name))
                        (string-suffix-p ".tsx" (buffer-name)))
                (setq emmet-expand-jsx-className? t)))))
