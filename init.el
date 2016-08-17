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
      vc-follow-symlinks t
      inhibit-startup-message t
      confirm-kill-emacs 'y-or-n-p
      global-auto-revert-mode t
      ring-bell-function 'ignore

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

      require-final-newline t
      tab-width 2
      indent-tabs-mode nil)

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

(setq-default frame-title-format "%b (%f)")
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":")

;;;
;; Packages
;;;

(use-package company
  :ensure t
  :init (global-company-mode)
  :config
  (setq company-idle-delay 0
	company-minimum-prefix-length 2
	company-selection-wrap-around t
	company-auto-complete t
	company-tooltip-align-annotations t
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

(use-package dracula-theme
  :ensure t
  :init (load-theme 'dracula t))

(use-package expand-region
  :ensure t
  :bind
  (("C-=" . er/expand-region)
   ("C-M-=" . er/contract-region)))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))

(use-package git-gutter
  :ensure t
  :init
  (global-git-gutter-mode +1)
  (git-gutter:linum-setup))

(use-package helm
  :ensure t
  :bind (("M-y" . helm-show-kill-ring)
	 ("C-x b" . helm-mini)
	 ("C-h SPC" . helm-all-mark-rings)
	 ("M-x" . helm-M-x))
  :config
  (setq helm-mode-fuzzy-match t
	helm-completion-in-region-fuzzy-match t
	helm-split-window-in-side-p t
	helm-move-to-line-cycle-in-source t
	elm-ff-file-name-history-use-recentf t)
  (helm-autoresize-mode 1))

(use-package helm-ag :ensure t)

(use-package helm-projectile :ensure t)

(use-package js2-mode
  :ensure t
  :mode "\\.js$"
  :config
  (add-hook 'js2-mode-hook '(setq js2-basic-offset 2
				  js2-indent-switch-body t
				  js2-highlight-level 3))
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

(use-package multiple-cursors
  :ensure t
  :bind
  (("M-=" . mc/edit-lines)
   ("C-S-<right>" . mc/mark-next-like-this)
   ("C-S-<left>" . mc/mark-previous-like-this)))

(use-package neotree
  :ensure t
  :config (global-set-key [f8] 'neotree-toggle))

(use-package nvm
  :ensure t)

(use-package org
  :ensure t
  :config
  (add-hook 'org-mode-hook '(setq org-src-fontify-natively t
				  org-default-notes-file "~/Documents/notes/notes.org"
				  org-agenda-files '("~/Documents/notes/notes.org"
						     "~/Documents/notes/private"
						     "~/Documents/notes/work")))
  (add-hook 'org-mode-hook (lambda () (add-to-list 'org-src-lang-modes '("javascript" . js2))))
  (add-hook 'org-mode-hook (lambda () (add-to-list 'org-src-lang-modes '("es" . es))))
  (add-hook 'org-mode-hook (lambda () (linum-mode -1)))
  (add-hook 'org-mode-hook (lambda () (global-set-key (kbd "C-c c") 'org-capture))))

(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode))

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (add-hook 'projectile-after-switch-project-hook 'iensu/use-local-eslint)
  (when (package-installed-p 'helm)
    (setq projectile-completion-system 'helm)
    (helm-projectile-on)))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

(use-package tern
  :ensure t
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
	web-mode-markup-indent-offset 2)
  (add-hook 'web-mode-hook 'er/add-web-mode-expansions)
  (add-hook 'web-mode-hook (lambda () (add-to-list 'company-backends 'company-tern)))
  (add-hook 'web-mode-hook 'iensu/pick-nodejs-version)
  (defadvice company-tern (before web-mode-set-up-ac-sources activate)
    (when (equal major-mode 'web-mode)
      (let* ((cur-language (web-mode-language-at-pos))
             (js? (or (string= cur-language "javascript")
                      (string= cur-language "jsx"))))
        (if js?
            (unless tern-mode (tern-mode))
          (if tern-mode (tern-mode -1)))))))

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets")))

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
