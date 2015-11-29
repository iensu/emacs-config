;;;;
;; Packages
;;;;

;; Define package repositories
(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))


;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
  '(;; makes handling lisp expressions much, much easier
    ;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
    paredit

    ;; key bindings and code colorization for Clojure
    ;; https://github.com/clojure-emacs/clojure-mode
    clojure-mode

    ;; extra syntax highlighting for clojure
    clojure-mode-extra-font-locking

    ;; integration with a Clojure REPL
    ;; https://github.com/clojure-emacs/cider
    cider

    ;; allow ido usage in as many contexts as possible. see
    ;; customizations/navigation.el line 23 for a description
    ;; of ido
    ido-ubiquitous
    ido-vertical-mode

    ;; Enhances M-x to allow easier execution of commands. Provides
    ;; a filterable list of possible commands in the minibuffer
    ;; http://www.emacswiki.org/emacs/Smex
    smex

    neotree

    ;; project navigation
    projectile

    ;; colorful parenthesis matching
    rainbow-delimiters
    whitespace

    ;; edit html tags like sexps
    tagedit

    ;; editing
    expand-region
    yasnippet
    ; react-snippets
    autopair
    flycheck

    ;; git integration
    magit
    git-gutter

    ;; autocomplete
    auto-complete
    company
    company-quickhelp

    ;; themes
    solarized-theme
    smyx-theme
    meacupla-theme
    paper-theme
    pastelmac-theme
    cyberpunk-theme

    ;; misc
    deferred
    popup
    iedit

    ;; Clojure etc
    ac-cider
    slamhound
    ; ac-cider-compliment
    ; cl-lib

    ;; Scheme
    ; racket-mode
    geiser

    ;; Haskell
    haskell-mode
    ; shm ; structured haskell mode
    ghc
    company-ghc
    hindent

    ;; Python
    elpy

    ;; Elixir
    elixir-mode
    alchemist

    ;; Groovy
    groovy-mode
    inf-groovy
    grails-mode
    gradle-mode

    ;; Latex
    auctex

    ;; Erlang
    edts

    ;; Elm
    elm-mode

    ;; javascript
    js2-mode
    js2-refactor
    web-beautify
    tern
    tern-auto-complete
    web-mode
    js-comint

    markdown-mode
    ; editorconfig

    adoc-mode
    ))

(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; Place downloaded elisp files in ~/.emacs.d/vendor. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;;
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
(add-to-list 'load-path "~/.emacs.d/vendor")


;;;;
;; Customization
;;;;

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; For editing lisps
(load "elisp-editing.el")

;; For auto-complete
(load "setup-ac.el")

;; Langauage-specific
(load "setup-clojure.el")
(load "setup-js.el")
(load "setup-haskell.el")
(load "setup-python.el")
(load "setup-elixir.el")
; (load "groovy-electric.el")
; (load "setup-groovy.el")
(load "setup-erlang.el")
(load "setup-bash.el")
(load "setup-org.el")
(load "setup-scheme.el")

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
