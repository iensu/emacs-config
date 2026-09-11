;; -*- lexical-binding: t; -*-

;; WebAssembly Text Format (WAT)
(use-package wat-mode
  :vc (wat-mode :url "https://github.com/devonsparks/wat-mode"
                :rev "46b4df83e92c585295d659d049560dbf190fe501")
  :config
  (add-to-list 'org-src-lang-modes '("wat" . wat)))

;; WebAssembly Interface Types (WIT)
(use-package wit-mode
  :vc (wit-mode :url "https://git.sr.ht/~iensu/wit-mode"
                :rev "41cf5e3ecaaf596dbd34338e24c239e2da73c7cf")
  :mode "\\.wit\\'"
  :config
  (add-to-list 'org-src-lang-modes '("wit" . wit)))
