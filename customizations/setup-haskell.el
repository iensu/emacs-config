;; Requires the following packages to be installed via cabal:
;;
;; happy
;; hasktags
;; stylish-haskell
;; present
;; ghc-mod
;; hlint
;; hoogle (remember to run 'hoogle data' in terminal to populate db)
;; structured-haskell-mode

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook
          (lambda () 
            (ghc-init)
            (haskell-indentation-mode)))
