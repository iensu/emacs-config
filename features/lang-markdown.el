;; -*- lexical-binding: t; -*-

(use-package markdown-ts-mode
  :mode ("\\.md\\'" "\\.mdx\\'" "\\.markdown\\'")
  :config
  (require 'markdown-ts-mode-x))

;; (use-package markdown-mode
;;   :commands (markdown-mode gfm-mode)
;;   :mode (("\\.md\\'"       . gfm-mode)
;;          ("\\.markdown\\'" . markdown-mode))
;;   :config
;;   (when (executable-find "multimarkdown")
;;     (setopt markdown-command "multimarkdown"))
;;   (add-to-list 'markdown-css-paths (concat user-emacs-directory "assets/markdown-css/styles.css")))

(use-package separedit
  :ensure t
  :config
  (setq separedit-default-mode 'markdown-mode))
