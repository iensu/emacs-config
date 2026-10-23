;; -*- lexical-binding: t; -*-

(add-hook 'python-mode-hook 'eglot-ensure)

(use-package python-black
  :ensure t
  :demand t
  :vc (python-black :url "https://github.com/wbolster/emacs-python-black"
                    :rev "779d49c7db54590d1fa483ef2f89eea5ef8774e1")
  :after python
  :config
  (add-hook 'python-mode-hook (lambda ()
                                (when (executable-find "black")
                                  (python-black-on-save-mode 1)))))
