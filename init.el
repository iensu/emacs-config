;;; init.el --- My configuration file

;;; Commentary:

;;; Code:

(package-initialize)

(setq user-full-name "Jens Östlund"
      user-mail-address "jostlund@gmail.com")

(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)

(unless (package-installed-p 'pallet)
  (package-install 'pallet))

(require 'pallet)
(pallet-mode t)

(require 'use-package)

(add-to-list 'package-archives
             '("elpy" . "https://jorgenschaefer.github.io/packages/"))

(defun iensu/emacs-config-file (filename)
  "Return FILENAME expanded to the Emacs configuration directory."
  (expand-file-name filename user-emacs-directory))

(load-file (iensu/emacs-config-file "iensu.el"))

;;;
;; Sane Defaults
;;;

(mapc
 (lambda (mode)
   (when (fboundp mode)
     (funcall mode -1)))
 '(menu-bar-mode tool-bar-mode scroll-bar-mode))

(setq default-directory "~/"
      custom-file (iensu/emacs-config-file "custom.el")
      vc-follow-symlinks t
      inhibit-startup-message t
      confirm-kill-emacs 'y-or-n-p
      global-auto-revert-mode t
      ring-bell-function 'ignore
      initial-scratch-message nil

      create-lockfiles nil
      auto-save-default nil
      backup-directory-alist `(("." . ,(iensu/emacs-config-file ".saves")))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6

      scroll-conservatively 0
      scroll-step 4

      mac-option-modifier nil
      mac-command-modifier 'meta
      mac-function-modifier 'hyper
      mac-right-option-modifier 'hyper

      require-final-newline t

      calendar-week-start-day 1)

(setq-default cursor-type '(bar . 2)

              indent-tabs-mode nil
              tab-width 2)

(fset 'yes-or-no-p 'y-or-n-p)

(when (memq window-system '(max ns))
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'prog-mode-hook
          '(lambda ()
             (subword-mode)
             (iensu/setup-line-numbers)
             (eldoc-mode)))

(global-prettify-symbols-mode t)

(delete-selection-mode t)
(global-font-lock-mode t)
(windmove-default-keybindings)
(winner-mode 1)

(global-hl-line-mode 1)

(setq-default frame-title-format "%b (%f)")
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":")

;; Setup org-related files and registers
(let ((private      "~/Dropbox/org/private.org")
      (work         "~/Dropbox/org/work.org")
      (notes        "~/Dropbox/org/notes.org")
      (refile       "~/Dropbox/org/refile.org")
      (journal      "~/Dropbox/org/journal.org")
      (appointments "~/Dropbox/org/appointments.org"))
  (progn
    (setq org-agenda-files (list private work notes refile journal appointments)
          org-default-notes-file notes
          org-directory "~/Dropbox/org/"
          org-capture-templates `(("t" "todo" entry (file ,refile)
                                   "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
                                  ("j" "Journal" entry (file+datetree ,journal)
                                   "* %?\n%U\n" :clock-in t :clock-resume t)
                                  ("l" "Link" entry (file+headline ,refile "Links")
                                   "* %? %^L %^g \n%T" :prepend t)
                                  ("L" "Link" entry (file+headline ,refile "Links")
                                   "*  %c\n%T" :prepend t :immediate-finish t)
                                  ("a" "Appointment" entry (file ,appointments)
                                   "* %^{title} %^G \nSCHEDULED: %^T\n\n%?")
                                  ("p" "Chrome Note" entry (file+headline ,refile "Chrome Notes")
                                   "* %^{Title}\n%T\n\n  Source: %u, %c\n\n  %i"
                                   :prepend t :immediate-finish t))
          org-todo-keywords '((sequence "TODO" "DOING" "|" "DONE")))
    (set-register ?p `(file . ,private))
    (set-register ?w `(file . ,work))
    (set-register ?n `(file . ,notes))
    (set-register ?j `(file . ,journal))
    (set-register ?r `(file . ,refile))
    (set-register ?a `(file . ,appointments))))

;; Global key bindings
(global-set-key (kbd "C-h C-s") 'iensu/toggle-scratch-buffer)
(global-set-key (kbd "C-h C-c") 'iensu/open-calendar-buffer)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "H-m") 'iensu/move-file)
(global-set-key (kbd "H-d") 'iensu/duplicate-line)
(global-set-key (kbd "H-t") 'toggle-truncate-lines)
(global-set-key (kbd "H-o") 'iensu/switch-to-minibuffer)
(global-set-key (kbd "H-x") 'yas-expand)
(global-set-key (kbd "H-e H-e") 'iensu/open-eshell-here)
(global-set-key (kbd "H-e H-x") 'iensu/close-eshell)
(global-set-key (kbd "M-i") 'imenu)
(global-set-key (kbd "M-o") 'occur)
(global-set-key [remap dabbrev-expand] 'hippie-expand)

;; Start Emacs server, which enables quick emacsclient access
(server-start)

;; On MacOS/OSX remember to disable the built in dictionary lookup command
;; by running the following command followed by a restart
;; defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 70 '<dict><key>enabled</key><false/></dict>'

;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;

;;; General packages

(use-package abbrev :diminish abbrev-mode)

(use-package all-the-icons
  :ensure t
  :defer nil)

(use-package all-the-icons-dired
  :ensure t
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))


(use-package beacon
  :ensure t
  :diminish beacon-mode
  :config
  (beacon-mode 1))

(use-package company
  :ensure t
  :diminish company-mode
  :init (global-company-mode)
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-auto-complete t
        company-tooltip-align-annotations t
        company-dabbrev-downcase nil
        company-auto-complete-chars nil)
  (add-hook 'emacs-lisp-mode-hook (lambda ()
                                    (add-to-list 'company-backends 'company-elisp)))
  (add-to-list 'company-transformers 'iensu/company-private-last-transformer)
  (eval-after-load 'company (company-quickhelp-mode 1)))

(use-package company-quickhelp
  :ensure t
  :config
  (setq company-quickhelp-delay 1)
  (define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin))

(use-package diminish
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "ελ")))
  (add-hook 'lisp-interaction-mode (lambda () (setq mode-name "λ")))
  (add-hook 'js2-mode-hook (lambda () (setq mode-name "js2"))))

(use-package dired+
  :ensure t
  :config
  (when (executable-find "gls") ;; native OSX ls works differently then GNU ls
    (setq insert-directory-program "/usr/local/bin/gls"))
  (setq dired-listing-switches "-alGh --group-directories-first"))

(use-package dracula-theme
  :ensure t
  :init
  (when (display-graphic-p)
    (load-theme 'dracula t))
  :config
  (set-face-attribute 'default nil :font "Anonymous Pro for Powerline" :height 190)
  (set-face-attribute 'region nil :background "#57145D" :foreground nil))

(use-package editorconfig
  :ensure t
  :diminish editorconfig-mode
  :init
  (add-hook 'prog-mode-hook (editorconfig-mode 1))
  (add-hook 'text-mode-hook (editorconfig-mode 1)))

(use-package eldoc :diminish eldoc-mode)

(use-package emojify
  :ensure t
  :init
  (add-hook 'text-mode-hook 'emojify-mode)
  (add-hook 'org-mode-hook 'emojify-mode))

(use-package expand-region
  :ensure t
  :bind
  (("C-=" . er/expand-region)
   ("C-M-=" . er/contract-region)))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))

(use-package gnus
  :ensure t
  :init
  (setq gnus-init-file (iensu/emacs-config-file ".gnus.el")))

(use-package hydra :ensure t)

(use-package iedit :ensure t)

(use-package multiple-cursors
  :ensure t
  :bind
  (("M-="         . mc/edit-lines)
   ("C-S-<right>" . mc/mark-next-like-this)
   ("C-S-<left>"  . mc/mark-previous-like-this)))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (when (executable-find "macdown")
    (setq  markdown-open-command "macdown")))

(use-package neotree
  :ensure t
  :config
  (global-set-key [f8] 'neotree-toggle)
  (setq neo-theme 'icons))

(use-package paren
  :ensure t
  :init
  (add-hook 'prog-mode-hook (lambda () (show-paren-mode t)))
  :config
  (setq show-paren-delay 0
        show-paren-when-point-inside-paren t)
  (set-face-attribute 'show-paren-match nil
                      :background nil
                      :foreground "#fc851e"
                      :weight 'extra-bold))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (projectile-global-mode)
  (add-hook 'projectile-after-switch-project-hook 'iensu/use-local-eslint))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'js2-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'elm-mode-hook 'rainbow-delimiters-mode))

(use-package recentf
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 20))

(use-package linum-relative
  :ensure t
  :bind (("H-l" . linum-relative-toggle)))

(use-package smart-mode-line
  :ensure t
  :init
  (setq sml/no-confirm-load-theme t)
  (sml/setup)
  :config
  (setq sml/theme 'dark))

(use-package smartparens
  :ensure t
  :init
  (smartparens-global-mode t)
  (require 'smartparens-config)
  :bind
  (:map smartparens-mode-map
        ("M-s" . sp-unwrap-sexp)
        ("C-<down>" . sp-down-sexp)
        ("C-<up>"   . sp-up-sexp)
        ("M-<down>" . sp-backward-down-sexp)
        ("M-<up>"   . sp-backward-up-sexp)
        ("C-<right>" . sp-forward-slurp-sexp)
        ("M-<right>" . sp-forward-barf-sexp)
        ("C-<left>"  . sp-backward-slurp-sexp)
        ("M-<left>"  . sp-backward-barf-sexp)))

(use-package sr-speedbar
  :ensure t
  :bind (("H-s" . sr-speedbar-toggle))
  :config
  (setq sr-speedbar-default-width 60)
  (speedbar-add-supported-extension ".elm"))

(use-package subword
  :diminish subword-mode
  :init
  (defadvice subword-transpose (before subword-transpose)
    (when (looking-at "$")
      (backward-word 1))))

(use-package time
  :init
  (display-time-mode t)
  (setq display-time-24hr-format t)
  :config
  (setq display-time-day-and-date nil))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init
  (yas-global-mode 1)
  (setq yas-snippet-dirs (add-to-list 'yas-snippet-dirs (iensu/emacs-config-file "snippets")))
  :config
  (add-hook 'snippet-mode-hook (lambda ()
                                 (setq mode-require-final-newline nil
                                       require-final-newline nil))))

;; ivy / counsel / swiper

(use-package counsel
  :ensure t
  :diminish ivy-mode
  :init
  (ivy-mode 1)
  :bind
  (("C-s" . swiper)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("<f1> f" . counsel-describe-function)
   ("<f1> v" . counsel-describe-variable)
   ("<f1> l" . counsel-find-library)
   ("<f2> i" . counsel-info-lookup-symbol)
   ("<f2> u" . counsel-unicode-char)
   ("C-c k" . counsel-ag)
   ("C-x l" . counsel-locate)
   ("C-x b" . ivy-switch-buffer)
   ("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line))
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "))

(use-package counsel-projectile :ensure t :init (counsel-projectile-on))

;; ido

(use-package ido
  :init
  (ido-mode t)
  :config
  (setq ido-everywhere t
        ido-enable-flex-matching t
        ido-use-filename-at-point 'guess
        ido-create-new-buffer 'always))

(use-package smex
  :ensure t)

;;; Git

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :init
  (global-git-gutter-mode +1)
  (git-gutter:linum-setup)
  (defhydra hydra-git-gutter ()
    "git-gutter"
    ("n" git-gutter:next-hunk "next")
    ("p" git-gutter:previous-hunk "prev")
    ("k" git-gutter:revert-hunk "revert")
    ("s" git-gutter:stage-hunk "stage")
    ("m" magit-status "magit-status" :exit t)
    ("g" git-gutter "refresh")
    ("q" nil "quit" :exit t))
  (global-set-key (kbd "C-h C-g") 'hydra-git-gutter/body))

(use-package git-timemachine :ensure t)

;;; Org-mode

(use-package org
  :ensure t
  :config
  (iensu/add-auto-mode 'org-mode "\\.trello$")
  (add-hook 'org-mode-hook (lambda ()
                             (setq org-src-fontify-natively t
                                   org-format-latex-options (plist-put org-format-latex-options :scale 1.5)
                                   truncate-lines nil
                                   org-image-actual-width nil)))
  (add-hook 'org-mode-hook (lambda () (add-to-list 'org-src-lang-modes '("javascript" . js2))))
  (add-hook 'org-mode-hook (lambda () (add-to-list 'org-src-lang-modes '("es" . es))))
  (add-hook 'org-mode-hook (lambda () (linum-mode -1)))
  (org-babel-do-load-languages
   'org-babel-load-languages '((emacs-lisp . t)
                               (sh . t)
                               (js . t)
                               (python . t)
                               (clojure . t)
                               (elixir . t)))
  (add-to-list 'org-modules 'org-protocol)
  (add-to-list 'org-modules 'ox-md)
  (org-load-modules-maybe t)
  (require 'org-protocol))

(use-package org-bullets
  :ensure t
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package ox-reveal
  :ensure t
  :init
  (add-to-list 'org-modules 'org-reveal)
  :config
  (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/"))

(use-package calfw :ensure t)
(use-package calfw-org :ensure t)

;;; Restclient

(use-package restclient
  :ensure t
  :config
  (iensu/add-auto-mode 'restclient-mode "\\.rest$")
  (add-hook 'restclient-mode-hook 'electric-indent-mode))

(use-package company-restclient
  :ensure t
  :config
  (add-hook 'restclient-mode-hook (lambda () (add-to-list 'company-backends 'company-restclient))))

(use-package ob-restclient
  :ensure t
  :init
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((restclient . t))))

(use-package docker-compose-mode :ensure t)
(use-package dockerfile-mode :ensure t)

;;;;;;;;;;;
;; Programming languages
;;;;;;;;;;;

;;; Clojure

(use-package clojure-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook (lambda () (paredit-mode t))))

(use-package cider
  :ensure t
  :config
  (add-hook 'cider-mode-hook (lambda () (paredit-mode t)))
  (add-hook 'cider-repl-mode-hook (lambda () (paredit-mode t))))

;;; Elixir

(setq-default my-prettify-symbols-alist '(("->" . ?→)
                                          ("<-" . ?←)
                                          ("|>" . ?▶)
                                          ("<|" . ?◀)))

(use-package elixir-mode
  :ensure t
  :config
  (add-hook 'elixir-mode-hook
            (lambda ()
              (setq prettify-symbols-alist my-prettify-symbols-alist))))

(use-package alchemist :ensure t)

(use-package ob-elixir :ensure t)

;;; Elm

(use-package elm-mode
  :ensure t
  :init
  (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
  (add-hook 'elm-mode-hook
            (lambda ()
              (setq company-backends '(company-elm))))
  (add-hook 'elm-mode-hook
            (lambda ()
              (setq prettify-symbols-alist my-prettify-symbols-alist)))
  :config
  (setq elm-indent-offset 2
        elm-tags-exclude-elm-stuff nil
        elm-tags-on-save t)
  (when (executable-find "elm-format")
    (setq elm-format-on-save t)))

(use-package flycheck-elm
  :ensure t
  :init
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-elm-setup)))

;;; Haskell

(use-package haskell-mode
  :ensure t
  :config
  (autoload 'ghc-init "ghc" nil t)
  (autoload 'ghc-debug "ghc" nil nil)
  (add-hook 'haskell-mode-hook (lambda () (ghc-init))))

(use-package ghc
  :ensure t
  :config
  (let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
    (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
    (add-to-list 'exec-path my-cabal-path))
  (custom-set-variables '(haskell-tags-on-save nil)))

(use-package hindent
  :ensure t
  :config
  (add-hook 'haskell-mode-hook #'hindent-mode))

;;; Javascript / Web

(use-package js2-mode
  :ensure t
  :mode "\\.js$"
  :bind (:map js2-mode-map
              (("C-k" . js2r-kill)))
  :config
  (setq js2-basic-offset 2
        js-switch-indent-offset 2
        js2-highlight-level 3)
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers '(javascript-jshint)))
  (add-hook 'js2-mode-hook
            (lambda ()
              (progn
                (js2-mode-hide-warnings-and-errors)
                (electric-indent-mode t)
                (iensu/pick-nodejs-version)
                (iensu/use-local-eslint)
                (js2-imenu-extras-mode)
                (js2-refactor-mode)
                (js2r-add-keybindings-with-prefix "C-c C-m")
                (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)
                (define-key js-mode-map (kbd "M-.") nil))))
  (flycheck-add-mode 'javascript-eslint 'js2-mode))

(use-package js2-refactor :ensure t)

(use-package xref-js2
  :ensure t
  :defer nil)

(use-package company-tern
  :ensure t
  :config
  (add-hook 'js2-mode-hook (lambda ()
                             (when (executable-find "tern")
                               (add-to-list 'company-backends 'company-tern)))))

(use-package css-mode
  :ensure t
  :config
  (setq css-indent-offset 2)
  (iensu/add-auto-mode 'css-mode "\\.styl$")
  (add-hook 'css-mode-hook (lambda ()
                             (rainbow-delimiters-mode)
                             (show-paren-mode))))

(use-package json-mode
  :ensure t
  :config
  (iensu/add-auto-mode 'json-mode "\\.json$")
  (setq js-indent-level 2))

(use-package mocha :ensure t)

(use-package nvm :ensure t)

(use-package tern
  :ensure t
  :diminish tern-mode " †"
  :config
  (when (executable-find "tern")
    (add-hook 'js2-mode-hook (lambda () (tern-mode t))))
  (define-key tern-mode-keymap (kbd "M-.") nil)
  (define-key tern-mode-keymap (kbd "M-,") nil)
  (iensu/add-auto-mode 'json-mode "\\.tern-project$"))

(use-package web-mode
  :ensure t
  :init
  (iensu/add-auto-mode 'web-mode "\\.html$" "\\.jsx$" "\\.hbs$" "\\.handlebars$")
  :config
  (setq web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-markup-indent-offset 2
        web-mode-attr-indent-offset 2
        web-mode-attr-value-indent-offset 2
        web-mode-enable-css-colorization t
        web-mode-enable-current-element-highlight t
        web-mode-enable-current-column-highlight t)
  (add-hook 'web-mode-hook #'(lambda () (yas-activate-extra-mode 'js-mode)))
  (add-hook 'web-mode-hook 'iensu/pick-nodejs-version)
  (add-hook 'web-mode-hook 'iensu/use-local-eslint)
  (setq-default flychqeck-disabled-checkers
                (append flycheck-disabled-checkers '(javascript-jshint)))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (when (executable-find "tern")
    (add-hook 'web-mode-hook (lambda ()
                               (add-to-list 'company-backends 'company-tern)))
    (defadvice company-tern (before web-mode-set-up-ac-sources activate)
      (when (equal major-mode 'web-mode)
        (let* ((cur-language (web-mode-language-at-pos))
               (js? (or (string= cur-language "javascript")
                        (string= cur-language "jsx"))))
          (if js?
              (unless tern-mode (tern-mode))
            (if tern-mode (tern-mode -1))))))))

(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'css-mode 'emmet-mode)
  (add-hook 'emmet-mode-hook (lambda ()
                               (when (string-suffix-p ".jsx" (buffer-name))
                                 (message "heeeeej")
                                 (setq emmet-expand-jsx-className? t))))
  (add-hook 'js2-jsx-mode (lambda () (setq emmet-expand-jsx-className? t))))

;;; OCaml

(use-package tuareg :ensure t)

;;; Python

(use-package elpy
  :ensure t
  :init
  (elpy-enable))

;;; Rust

(use-package rust-mode
  :ensure t
  :bind (:map rust-mode-map
         ("C-c <tab>" . rust-format-buffer))
  :config
  (iensu/add-auto-mode 'rust-mode "\\.rs$"))

(use-package cargo
  :ensure t
  :config
  (add-hook 'rust-mode-hook #'cargo-minor-mode))

(use-package flycheck-rust
	     :ensure t
	     :config
	     (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package racer
  :ensure t
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'rust-mode-hook #'eldoc-mode)
  (setq racer-cmd "~/.cargo/bin/racer"
        racer-rust-src-path "/usr/local/src/rustc-1.8.0/src"))

(use-package company-racer
  :ensure t
  :config
  (add-hook 'rust-mode-hook (lambda ()
                              (add-to-list 'company-backends 'company-racer)))
  (setq company-tooltip-align-annotations t
        company-racer-rust-src "/usr/local/src/rustc-1.8.0/src"))

;;; Scala

(use-package scala-mode
  :ensure t)

(use-package sbt-mode
  :ensure t)

(use-package ensime
  :ensure t
  :config
  (add-hook 'ensime-mode-hook (lambda ()
                                (add-to-list 'company-backends 'ensime-company))))

;;; End packages

(load custom-file 'noerror)

(provide 'init)
;;; init.el ends here
(put 'narrow-to-region 'disabled nil)
