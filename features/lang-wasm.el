(use-package wat-mode
  :vc (wat-mode :url "https://github.com/devonsparks/wat-mode")
  :config
  (add-to-list 'org-src-lang-modes '("wat" . wat)))
