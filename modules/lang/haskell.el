;;; lang/haskell.el --- Haskell setup

(use-package haskell-mode
  :config
  (autoload 'ghc-init "ghc" nil t)
  (autoload 'ghc-debug "ghc" nil nil)
  (add-hook 'haskell-mode-hook (lambda () (ghc-init))))

(use-package ghc
  :config
  (let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
    (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
    (add-to-list 'exec-path my-cabal-path))
  (custom-set-variables '(haskell-tags-on-save nil)))

(use-package hindent
  :config
  (add-hook 'haskell-mode-hook #'hindent-mode))
