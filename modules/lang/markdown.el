;;; modules/lang/markdown.el --- Markdown

;;; Code:

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (when (executable-find "macdown")
    (setq  markdown-open-command "macdown"))
  (when (or (executable-find "ispell")
            (executable-find "aspell"))
    (flyspell-mode 1)))
