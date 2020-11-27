(use-package java-mode
  :mode "\\.java$"
  :hook
  (java-mode-hook . electric-pair-mode))

(use-package lsp-java
  :after lsp
  :hook
  (java-mode . lsp))
