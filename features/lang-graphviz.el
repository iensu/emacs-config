(use-package graphviz-dot-mode
  :bind (:map graphviz-dot-mode-map
              ("C-c C-c" . graphviz-dot-preview))
  :hook
  (graphviz-dot-mode . lsp-deferred)
  :init
  (require 'lsp-dot)
  :config
  (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))
  (setopt graphviz-dot-preview-extension "svg"))
