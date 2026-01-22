(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("\\.md\\'"       . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (when (executable-find "multimarkdown")
    (setopt markdown-command "multimarkdown"))
  (add-to-list 'markdown-css-paths (concat user-emacs-directory "assets/markdown-css/styles.css")))

(use-package markdown-toc)

(use-package separedit
  :config
  (setq separedit-default-mode 'markdown-mode))
