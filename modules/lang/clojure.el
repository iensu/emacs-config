;;; lang/clojure.el --- Clojure

(use-package clojure-mode
  :config
  (add-hook 'clojure-mode-hook (lambda () (paredit-mode t))))

(use-package cider
  :config
  (add-hook 'cider-mode-hook (lambda () (paredit-mode t)))
  (add-hook 'cider-repl-mode-hook (lambda () (paredit-mode t))))
