;; -*- lexical-binding: t; -*-

(use-package geiser
  :ensure t
  :vc (geiser :url "https://github.com/emacsmirror/geiser"
              :rev "1d7b08e989df3933ea0b4414ebfbdd99c3cc24ed")
  :hook
  (geiser-repl-mode . smartparens-strict-mode)
  :config
  (geiser-autodoc-mode 1))

(use-package geiser-guile
  :ensure t
  :vc (geiser-guile :url "https://github.com/emacsmirror/geiser-guile"
                    :rev "fd732bfb4c7c7881ab50bf58c15c9348bd1840db"))

(defun iensu-scheme-hook ()
  (smartparens-strict-mode 1)
  (eldoc-mode 1))

(add-hook 'scheme-mode-hook 'iensu-scheme-hook)
