(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("\\.md\\'"       . gfm-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package markdown-toc)
