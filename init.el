(setq exec-path-from-shell-check-startup-files nil)

(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(
    paredit

    clojure-mode
    clojure-mode-extra-font-locking
    cider
    ac-cider
    slamhound

    ido-ubiquitous
    ido-vertical-mode

    smex

    neotree

    projectile

    rainbow-delimiters

    whitespace

    tagedit

    expand-region

    yasnippet

    autopair

    flycheck

    magit
    git-gutter

    auto-complete
    company
    ; company-quickhelp

    solarized-theme
    smyx-theme
    meacupla-theme
    paper-theme
    pastelmac-theme
    cyberpunk-theme

    deferred
    popup
    iedit

    geiser

    haskell-mode
    ; shm ; structured haskell mode
    ghc
    company-ghc
    hindent

    elpy

    elixir-mode
    alchemist

    groovy-mode

    auctex

    edts

    elm-mode

    js2-mode
    js2-refactor
    web-beautify
    tern
    tern-auto-complete
    web-mode
    json-mode
    restclient
    mocha-snippets

    markdown-mode

    adoc-mode

    ag

    multiple-cursors
    ))

(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-to-list 'load-path "~/.emacs.d/vendor")
(add-to-list 'load-path "~/.emacs.d/customizations")

(load "shell-integration.el")
(load "navigation.el")
(load "ui.el")
(load "editing.el")
(load "misc.el")
(load "elisp-editing.el")
(load "setup-ac.el")
(load "setup-clojure.el")
(load "setup-js.el")
(load "setup-haskell.el")
(load "setup-python.el")
(load "setup-elixir.el")
(load "setup-erlang.el")
(load "setup-bash.el")
(load "setup-org.el")

(when (file-exists-p "~/.emacs.d/customizations/local-overrides.el")
  (progn
    (message "Applying local overrides...")
    (load "local-overrides.el")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (flycheck web-mode web-beautify tern-auto-complete tagedit solarized-theme smex slamhound rainbow-delimiters projectile paredit magit js2-refactor inf-groovy iedit ido-vertical-mode ido-ubiquitous groovy-mode grails-mode gradle-mode git-rebase-mode git-commit-mode expand-region elm-mode elixir-mode edts deferred clojure-mode-extra-font-locking autopair auctex alchemist ac-cider))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
