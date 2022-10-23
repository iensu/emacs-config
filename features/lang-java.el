(use-package java-mode
  :mode "\\.java$"
  :hook
  (java-mode-hook . electric-pair-mode)
  (java-mode-hook . eglot-ensure))
