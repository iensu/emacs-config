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

      require-final-newline t

      calendar-week-start-day 1)

(setq-default cursor-type '(bar . 2)

              indent-tabs-mode nil
              tab-width 2)

(fset 'yes-or-no-p 'y-or-n-p)

(when (memq window-system '(max ns))
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs '("PATH")))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'prog-mode-hook
          '(lambda ()
             (subword-mode)
             (prettify-symbols-mode)
             (iensu/setup-line-numbers)
             (eldoc-mode)))

(delete-selection-mode t)
(global-font-lock-mode t)
(windmove-default-keybindings)

(setq-default frame-title-format "%b (%f)")
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":")

;; Setup org-related files and registers
(let ((private      "~/Documents/notes/private.org")
      (work         "~/Documents/notes/work.org")
      (notes        "~/Documents/notes/notes.org")
      (refile       "~/Documents/notes/refile.org")
      (journal      "~/Documents/notes/journal.org")
      (appointments "~/Documents/notes/appointments.org"))
  (progn
    (setq org-agenda-files (list private work notes refile journal appointments)
          org-default-notes-file notes
          org-capture-templates `(("t" "todo" entry (file ,refile)
                                   "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
                                  ("j" "Journal" entry (file+datetree ,journal)
                                   "* %?\n%U\n" :clock-in t :clock-resume t)
                                  ("l" "Link" entry (file+headline ,refile "Links")
                                   "* %? %^L %^g \n%T" :prepend t)
                                  ("L" "Link" entry (file+headline ,refile "Links")
                                   "*  %c\n%T" :prepend t :immediate-finish t)
                                  ("a" "Appointment" entry (file ,appointments)
                                   "* %? %^G \nSCHEDULED: %^T")
                                  ("p" "Chrome Note" entry (file+headline ,refile "Chrome Notes")
                                   "* %^{Title}\n%T\n\n  Source: %u, %c\n\n  %i"
                                   :prepend t :immediate-finish t)))
    (set-register ?p `(file . ,private))
    (set-register ?w `(file . ,work))
    (set-register ?n `(file . ,notes))
    (set-register ?j `(file . ,journal))
    (set-register ?r `(file . ,refile))
    (set-register ?a `(file . ,appointments))))

;; Global key bindings
(global-set-key (kbd "C-j") 'newline-and-indent)
(global-set-key (kbd "C-a") 'back-to-indentation)
(global-set-key (kbd "C-<return>") 'open-line)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-h C-s") 'iensu/toggle-scratch-buffer)
(global-set-key (kbd "C-h C-c") 'iensu/open-calendar-buffer)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-M-<backspace>") 'just-one-space)
(global-set-key (kbd "H-m") 'iensu/move-file)
(global-set-key (kbd "H-d") 'iensu/duplicate-line)
(global-set-key (kbd "H-t") 'toggle-truncate-lines)

;; Start Emacs server, which enables quick emacsclient access
(server-start)

;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;

;;; General packages

(use-package abbrev :diminish abbrev-mode)

(use-package avy
  :ensure t
  :config
  (global-set-key (kbd "C-:") 'avy-goto-char-timer))

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
  (set-face-attribute 'default nil :font "Anonymous Pro for Powerline" :height 140)
  (set-face-attribute 'region nil :background "#57145D" :foreground nil))

(use-package editorconfig
  :ensure t
  :init
  (add-hook 'prog-mode-hook (editorconfig-mode 1))
  (add-hook 'text-mode-hook (editorconfig-mode 1)))

(use-package eldoc :diminish eldoc-mode)

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

(use-package hlinum
  :ensure t
  :config
  (hlinum-activate)
  (set-face-attribute 'linum-highlight-face nil :foreground "yellow" :background nil :height 120))

(use-package hydra :ensure t)

(use-package iedit :ensure t)

(use-package multiple-cursors
  :ensure t
  :bind
  (("M-=" . mc/edit-lines)
   ("C-S-<right>" . mc/mark-next-like-this)
   ("C-S-<left>" . mc/mark-previous-like-this)))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package neotree
  :ensure t
  :config (global-set-key [f8] 'neotree-toggle))

(use-package nyan-mode
  :ensure t
  :config
  (nyan-mode))

(use-package origami
  :ensure t
  :init (global-origami-mode)
  :bind (("<f6>" . origami-recursively-toggle-node)
         ("<f7>" . origami-toggle-all-nodes)))

(use-package paredit
  :ensure t
  :diminish paredit-mode " π"
  :config
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode))

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

(use-package w3m :ensure t)

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init
  (yas-global-mode 1)
  (setq yas-snippet-dirs (add-to-list 'yas-snippet-dirs (iensu/emacs-config-file "snippets"))))

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

;;; Ivy

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :bind (("C-c C-r" . ivy-resume)
         ("<f6>" . ivy-resume))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-height 10
        ivy-initial-inputs-alist nil))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         ("C-x C-f" . counsel-find-file)
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> l" . counsel-load-library)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-ag)
         ("C-x l" . counsel-locate)
         ("C-S-o" . counsel-rythmbox)
         :map read-expression-map
         ("C-r" . counsel-expression-history)
         :map ivy-minibuffer-map
         ("M-y" . ivy-next-line-and-call)
         ("M-Y" . ivy-previous-line-and-call))
  :config
  (setq counsel-find-file-at-point t))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-on))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)))

;;; Org-mode

(use-package org
  :ensure t
  :config
  (iensu/add-auto-mode 'org-mode "\\.trello$")
  (add-hook 'org-mode-hook (lambda ()
                             (setq org-src-fontify-natively t
                                   org-format-latex-options (plist-put org-format-latex-options :scale 1.5)
                                   truncate-lines nil)))
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
  (org-load-modules-maybe t)
  (require 'org-protocol))

(use-package org-bullets
  :ensure t
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package calfw
  :ensure t
  :init
  (require 'calfw-org))

;;; Restclient

(use-package restclient
  :ensure t
  :config
  (iensu/add-auto-mode 'restclient-mode "\\.rest$")
  (add-hook 'restclient-mode-hook 'electric-pair-mode)
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

(use-package elixir-mode :ensure t)

(use-package alchemist :ensure t)

(use-package ob-elixir :ensure t)

;;; Elm

(use-package elm-mode
  :ensure t
  :init
  (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
  (add-hook 'elm-mode-hook 'electric-pair-mode)
  (add-hook 'elm-mode-hook
            (lambda ()
              (setq company-backends '(company-elm))))
  :config
  (setq elm-indent-offset 2
        elm-tags-exclude-elm-stuff nil)
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
  :config
  (add-hook 'js2-mode-hook (lambda ()
                             (setq js2-basic-offset 2
                                   js2-indent-switch-body t
                                   js2-highlight-level 3)))
  (add-hook 'js2-mode-hook 'js2-mode-hide-warnings-and-errors)
  (add-hook 'js2-mode-hook 'electric-pair-mode)
  (add-hook 'js2-mode-hook (lambda () (electric-indent-mode t)))
  (add-hook 'js2-mode-hook 'iensu/pick-nodejs-version)
  (add-hook 'js2-mode-hook 'iensu/use-local-eslint)
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers '(javascript-jshint)))
  (flycheck-add-mode 'javascript-eslint 'js2-mode))

(use-package js2-refactor
  :ensure t
  :config
  (add-hook 'js2-mode-hook 'js2-refactor-mode)
  (add-hook 'js2-mode-hook (lambda ()
                             (js2r-add-keybindings-with-prefix "C-c C-m"))))

(use-package company-tern
  :ensure t
  :config
  (add-hook 'js2-mode-hook (lambda () (add-to-list 'company-backends 'company-tern))))

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
  (add-hook 'js2-mode-hook (lambda () (tern-mode t)))
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
  (add-hook 'web-mode-hook (lambda () (add-to-list 'company-backends 'company-tern)))
  (add-hook 'web-mode-hook 'iensu/pick-nodejs-version)
  (add-hook 'web-mode-hook 'iensu/use-local-eslint)
  (setq-default flychqeck-disabled-checkers
                (append flycheck-disabled-checkers '(javascript-jshint)))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (defadvice company-tern (before web-mode-set-up-ac-sources activate)
    (when (equal major-mode 'web-mode)
      (let* ((cur-language (web-mode-language-at-pos))
             (js? (or (string= cur-language "javascript")
                      (string= cur-language "jsx"))))
        (if js?
            (unless tern-mode (tern-mode))
          (if tern-mode (tern-mode -1)))))))

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
  :ensure t
  :config
  (add-hook 'scala-mode-hook 'electric-pair-mode))

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
