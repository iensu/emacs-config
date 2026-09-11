;; -*- lexical-binding: t; -*-

(use-package markdown-ts-mode
  :mode ("\\.md\\'" "\\.mdx\\'" "\\.markdown\\'")
  :config
  (require 'markdown-ts-mode-x))

(use-package separedit
  :ensure t
  :config
  (setq separedit-default-mode 'markdown-mode))
