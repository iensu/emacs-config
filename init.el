;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE INIT
;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar packages
  '(magit
    git-gutter

    helm
    helm-projectile
    projectile
    flycheck
    neotree

    company
    paredit
    multiple-cursors
    expand-region

    yasnippet
    mocha-snippets

    smyx-theme

    web-mode
    js2-mode
    js2-refactor
    web-beautify
    tern
    company-tern
    json-mode

    clojure-mode
    clojure-mode-extra-font-locking
    cider

    elixir-mode
    alchemist
    ))

(if (eq system-type 'darwin)
    (add-to-list 'packages 'exec-path-from-shell))

(dolist (p packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'cl)

;;;;;;;;;;;;;;;;;;;;;
;; BASIC SETTINGS
;;;;;;;;;;;;;;;;;;;;;
(fset 'yes-or-no-p 'y-or-n-p)

;; OSX related stuff
(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)
(when (package-installed 'exec-path-from-shell)
  (setq exec-path-from-shell-check-startup-files nil))
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH")))

;;;;;;;;;;;;;;;;;;;;;;
;; PROJECT RELATED
;;;;;;;;;;;;;;;;;;;;;;
(require 'helm)
(require 'helm-config)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)

(setq helm-split-window-inside-p t
      helm-move-to-line-cycle-in-source t
      helm-ff-file-name-history-use-recentf t
      helm-M-x-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t
      helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match t
      helm-locate-fuzzy-match t
      helm-apropos-fuzzy-match t
      helm-lisp-fuzzy-completion t)

(helm-mode 1)

(require 'projectile)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

(setq inhibit-startup-message t)

;;;;;;;;;;;;;;;;;;;;;
;; EDITING
;;;;;;;;;;;;;;;;;;;;;
(define-key key-translation-map [dead-grave]
  (lookup-key key-translation-map "\C-x8`"))
(define-key key-translation-map [dead-acute]
  (lookup-key key-translation-map "\C-x8'"))
(define-key isearch-mode-map [dead-grave] nil)
(define-key isearch-mode-map [dead-acute] nil)

(setq create-lockfiles nil
      auto-save-default nil
      x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t)

(global-auto-revert-mode 1)

(add-hook 'before-save-hook 'whitespace-cleanup)

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'yasnippet)
(yas-global-mode 1)

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-M-=") 'er/contract-region)

;;;;;;;;;;;;;;;;;;;;;
;; UI
;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
(load-theme 'smyx t)

(show-paren-mode 1)

(global-hl-line-mode 1)

(global-prettify-symbols-mode 1)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(setq ring-bell-function 'ignore)

(global-linum-mode)
(column-number-mode 1)

(setq-default frame-title-format "%b (%f)")

;;;;;;;;;;;;;;;;;;;;;
;; GIT
;;;;;;;;;;;;;;;;;;;;;
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

(require 'git-gutter)
(global-git-gutter-mode t)
(git-gutter:linum-setup)

;;;;;;;;;;;;;;;;;;;;;
;; LANGUAGE SETTINGS
;;;;;;;;;;;;;;;;;;;;;

;; bash
(add-to-list 'auto-mode-alist '("\\.env$" . sh-mode))
(add-hook 'sh-mode-hook
	  (lambda ()
	     (setq sh-basic-offset 2)
	     (setq sh-indentation 2)))

;; elisp
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (turn-on-eldoc-mode)
	    (enable-paredit-mode)
	    (add-to-list 'company-backends 'company-elisp)))

;; web-related (javascript, html etc)
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.dust$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.handlebars$" . web-mode))

(add-hook 'web-mode
	  (lambda ()
	    (subword-mode)
	    (setq web-mode-css-indent-offset 2
		  web-mode-code-indent-offset 2
		  web-mode-markup-indent-offset 2)))

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook
	  (lambda ()
	    (subword-mode)
	    (tern-mode t)
	    (add-to-list 'company-backends 'company-tern)
	    (js2-refactor-mode)
	    (electric-pair-mode)
	    (setq electric-indent-mode t
		  js2-basic-offset 2
		  js2-highlight-level 3
		  js2-indent-switch-body t)))


(require 'json-mode)
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

(add-to-list 'auto-mode-alist '("\\.styl" . css-mode))

(setq-default flycheck-disabled-checkers
	      (append flycheck-disabled-checkers
		      '(javascript-jshint)))
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-mode 'javascript-eslint 'js2-mode)

;; clojure
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.clj[scx]?$" . clojure-mode))
(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))

(require 'clojure-mode-extra-font-locking)
(add-hook 'clojure-mode-hook
	  (lambda ()
	    (enable-paredit-mode)
	    (subword-mode)
	    (prettify-symbols-mode)
	    (setq inferior-lisp-program "lein repl")))
(add-hook 'cider-mode-hook
	  (lambda ()
	    (setq cider-repl-pop-to-buffer-on-connect t
		  cider-show-error-buffer t
		  cider-auto-select-error-buffer t
		  cider-repl-history-file "~/.emacs.d/cider-history"
		  cider-repl-wrap-history t)
	    (cider-turn-on-eldoc-mode)
	    (add-to-list 'company-backends 'company-cider)))

(add-hook 'cider-repl-mode-hook
	  (lambda ()
	    (add-to-list 'company-backends 'company-cider)))

;; elixir
(require 'elixir-mode)
(require 'alchemist)

;;;;;;;;;;;;;;;;;;;;;;
;; LOCAL OVERRIDES
;;;;;;;;;;;;;;;;;;;;;;
(when (file-exists-p "~/.emacs.d/local-overrides.el")
  (progn
    (message "Applying local overrides...")
    (load "local-overrides.el")))
