(use-package graphviz-dot-mode
  :bind (:map graphviz-dot-mode-map
              ("C-c C-c" . graphviz-dot-preview))
  :config
  (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))
  (add-to-list 'org-babel-load-languages '(dot . t)))
