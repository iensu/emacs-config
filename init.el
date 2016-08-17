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
  :config (progn
	    (setq company-idle-delay 0
		  company-minimum-prefix-length 2
		  company-selection-wrap-around t
		  company-auto-complete t
		  company-tooltip-align-annotations t
		  company-auto-complete-chars nil)
	    (eval-after-load 'company
	      (use-package company-quickhelp
		:ensure t
		:init (company-quickhelp-mode 1)
		:config (progn
			  (setq company-quickhelp-delay 1)
			  (define-key company-active-map
			    (kbd "M-h")
			    #'company-quickhelp-manual-begin))))
	    (add-hook 'emacs-lisp-mode-hook '(add-to-list 'company-backends 'company-elisp))))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode t))

(use-package git-gutter
  :ensure t
  :init (progn
	  (global-git-gutter-mode +1)
	  (git-gutter:linum-setup)))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package neotree
  :ensure t
  :config (global-set-key [f8] 'neotree-toggle))

(use-package org
  :ensure t
  :config (progn
	    (add-hook 'org-mode-hook
		      (progn
			(setq org-src-fontify-natively t
			      org-default-notes-file "~/Documents/notes/notes.org"
			      org-agenda-files '("~/Documents/notes/notes.org"
						 "~/Documents/notes/private"
						 "~/Documents/notes/work"))
			(add-to-list 'org-src-lang-modes '("javascript" . js2))
			(add-to-list 'org-src-lang-modes '("es" . es))
			(linum-mode -1)
			(global-set-key (kbd "C-c c") 'org-capture)))))

(use-package paredit
  :ensure t
  :config (progn
	    (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)))

(use-package projectile
  :ensure t
  :config (projectile-global-mode))

(use-package rainbow-delimiters
  :ensure t
  :config (progn
	    (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)))

(provide 'init)
;;; init.el ends here
