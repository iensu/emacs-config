;; WebAssembly Text Format (WAT)
(use-package wat-mode
  :vc (wat-mode :url "https://github.com/devonsparks/wat-mode")
  :config
  (add-to-list 'org-src-lang-modes '("wat" . wat)))

;; WebAssembly Interface Types (WIT)
(use-package wit-mode
  :vc (wit-mode :url "https://git.sr.ht/~iensu/wit-mode")
  :mode "\\.wit\\'"
  :config
  (add-to-list 'org-src-lang-modes '("wit" . wit)))
