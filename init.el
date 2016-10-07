;;; init.el --- My configuration file

;;; Commentary:

;;; Code:

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

(load-file "~/.emacs.d/iensu.el")

;;;
;; Sane Defaults
;;;

(mapc
 (lambda (mode)
   (when (fboundp mode)
     (funcall mode -1)))
 '(menu-bar-mode tool-bar-mode scroll-bar-mode))

(setq default-directory "~/"
      custom-file "~/.emacs.d/custom.el"
      vc-follow-symlinks t
      inhibit-startup-message t
      confirm-kill-emacs 'y-or-n-p
      global-auto-revert-mode t
      ring-bell-function 'ignore
      initial-scratch-message nil

      create-lockfiles nil
      auto-save-default nil
      backup-directory-alist '(("." . "~/.emacs.d/.saves"))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6

      scroll-conservatively 0
      scroll-step 4

      mac-option-modifier nil
      mac-command-modifier 'meta

      require-final-newline t)

(setq-default indent-tabs-mode nil
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

(global-set-key (kbd "C-c d") 'iensu/duplicate-line)
(global-set-key (kbd "C-j") 'newline-and-indent)
(global-set-key (kbd "C-a") 'back-to-indentation)
(global-set-key (kbd "C-<return>") 'open-line)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; practice proper navigation
(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))

;;;
;; Packages
;;;

(use-package abbrev :diminish abbrev-mode)

(use-package cider
  :ensure t
  :config
  (add-hook 'cider-mode-hook (lambda () (paredit-mode t)))
  (add-hook 'cider-repl-mode-hook (lambda () (paredit-mode t))))

(use-package clojure-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook (lambda () (paredit-mode t))))

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

(use-package company-tern
  :ensure t
  :config
  (add-hook 'js2-mode-hook (lambda () (add-to-list 'company-backends 'company-tern))))

(use-package css-mode
  :ensure t
  :config
  (setq css-indent-offset 2)
  (iensu/add-auto-mode 'css-mode "\\.styl$"))

(use-package diminish
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "ελ")))
  (add-hook 'lisp-interaction-mode (lambda () (setq mode-name "λ")))
  (add-hook 'js2-mode-hook (lambda () (setq mode-name "js2"))))

(use-package dracula-theme
  :ensure t
  :init
  (when (display-graphic-p)
    (load-theme 'dracula t)))

(use-package editorconfig
  :ensure t
  :init
  (add-hook 'prog-mode-hook (editorconfig-mode 1))
  (add-hook 'text-mode-hook (editorconfig-mode 1)))

(use-package eldoc :diminish eldoc-mode)

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
        elm-tags-exclude-elm-stuff nil))

(use-package elpy
  :ensure t
  :init
  (elpy-enable))

(use-package expand-region
  :ensure t
  :bind
  (("C-=" . er/expand-region)
   ("C-M-=" . er/contract-region)))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))

(use-package flycheck-elm
  :ensure t
  :init
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-elm-setup)))

(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :init
  (global-git-gutter-mode +1)
  (git-gutter:linum-setup))

(use-package helm
  :ensure t
  :diminish helm-mode
  :bind (("M-y" . helm-show-kill-ring)
	 ("C-x b" . helm-mini)
	 ("C-h SPC" . helm-all-mark-rings)
	 ("C-x C-f" . helm-find-files)
	 ("M-x" . helm-M-x))
  :config
  (setq helm-mode-fuzzy-match t
	helm-completion-in-region-fuzzy-match t
	helm-split-window-in-side-p t
	helm-move-to-line-cycle-in-source t
	elm-ff-file-name-history-use-recentf t)
  (helm-autoresize-mode 1)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action))

(use-package helm-ag :ensure t)

(use-package helm-projectile :ensure t)

(use-package js2-mode
  :ensure t
  :mode "\\.js$"
  :config
  (add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2
				       js2-indent-switch-body t
				       js2-highlight-level 3)))
  (add-hook 'js2-mode-hook 'js2-mode-hide-warnings-and-errors)
  (add-hook 'js2-mode-hook 'electric-pair-mode)
  (add-hook 'js2-mode-hook (lambda () (electric-indent-mode t)))
  (add-hook 'js2-mode-hook 'iensu/pick-nodejs-version)
  (setq-default flycheck-disabled-checkers
		(append flycheck-disabled-checkers '(javascript-jshint)))
  (flycheck-add-mode 'javascript-eslint 'js2-mode))

(use-package js2-refactor
  :ensure t
  :config
  (add-hook 'js2-mode-hook 'js2-refactor-mode)
  (add-hook 'js2-mode-hook (lambda ()
			     (js2r-add-keybindings-with-prefix "C-c C-m"))))

(use-package json-mode
  :ensure t
  :config
  (iensu/add-auto-mode 'json-mode "\\.json$")
  (setq js-indent-level 2))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package mocha :ensure t)

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

(use-package nvm
  :ensure t)

(use-package org
  :ensure t
  :config
  (iensu/add-auto-mode 'org-mode "\\.trello$")
  (add-hook 'org-mode-hook (lambda ()
                             (setq org-src-fontify-natively t
                                   org-default-notes-file "~/Documents/notes/notes.org"
                                   org-agenda-files '("~/Documents/notes/notes.org"
                                                      "~/Documents/notes/private"
                                                      "~/Documents/notes/work"))))
  (add-hook 'org-mode-hook (lambda () (add-to-list 'org-src-lang-modes '("javascript" . js2))))
  (add-hook 'org-mode-hook (lambda () (add-to-list 'org-src-lang-modes '("es" . es))))
  (add-hook 'org-mode-hook (lambda () (linum-mode -1)))
  (add-hook 'org-mode-hook (lambda () (global-set-key (kbd "C-c c") 'org-capture)))
  (add-to-list 'org-src-lang-modes '("javascript" . js2)))

(use-package org-trello :ensure t)

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

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (projectile-global-mode)
  (add-hook 'projectile-after-switch-project-hook 'iensu/use-local-eslint)
  (when (package-installed-p 'helm)
    (setq projectile-completion-system 'helm)
    (helm-projectile-on)))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'js2-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'elm-mode-hook 'rainbow-delimiters-mode))

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

(use-package tern
  :ensure t
  :diminish tern-mode " †"
  :config
  (add-hook 'js2-mode-hook (lambda () (tern-mode t)))
  (iensu/add-auto-mode 'json-mode "\\.tern-project$"))

(use-package time
  :init
  (display-time-mode t)
  (setq display-time-24hr-format t)
  :config
  (setq display-time-day-and-date nil))

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

(use-package undo-tree :diminish undo-tree-mode)

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init
  (yas-global-mode 1)
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets")))

(load custom-file 'noerror)

(provide 'init)
;;; init.el ends here
