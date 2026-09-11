(use-package graphviz-dot-mode
  :ensure t
  :vc (graphviz-dot-mode :url "https://github.com/ppareit/graphviz-dot-mode"
                         :rev "0a4509e9f63c8eae8d050acc62eac642fd77ae59")
  :bind (:map graphviz-dot-mode-map
              ("C-c C-c" . graphviz-dot-preview))
  :hook
  (graphviz-dot-mode . lsp-deferred)
  :init
  (require 'lsp-dot)
  :config
  (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))
  (setopt graphviz-dot-preview-extension "svg"))
