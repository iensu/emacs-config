(use-package toml-mode
  :mode ("\\.toml$" "_redirects$")
  :hook
  (toml-mode . electric-pair-mode))
