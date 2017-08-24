;; modules/core/navigation.el --- Navigation

;;; Code:

(use-package dired+
  :ensure t
  :config
  (when (executable-find "gls") ;; native OSX ls works differently then GNU ls
    (setq insert-directory-program "/usr/local/bin/gls"))
  (setq dired-listing-switches "-alGh --group-directories-first"))

(use-package neotree
  :ensure t
  :config
  (global-set-key [f8] 'neotree-toggle)
  (setq neo-theme 'icons))

(use-package projectile
  :ensure t
  :delight '(:eval (concat " [" (projectile-project-name) "]"))
  :init
  (setq projectile-cache-file (iensu/-config-file ".local/projectile.cache")
        projectile-known-projects-file (iensu/-config-file ".local/projectile-bookmarks.eld"))
  :config
  (projectile-global-mode)
  (add-hook 'projectile-after-switch-project-hook 'iensu/use-local-eslint))

(use-package recentf
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 20))

(use-package sr-speedbar
  :ensure t
  :bind (("H-s" . sr-speedbar-toggle))
  :config
  (setq sr-speedbar-default-width 60)
  (speedbar-add-supported-extension ".elm"))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

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

(use-package ido
  :init
  (ido-mode t)
  :config
  (setq ido-everywhere t
        ido-enable-flex-matching t
        ido-use-filename-at-point 'guess
        ido-create-new-buffer 'always
        ido-save-directory-list-file (iensu/-config-file ".local/ido.last")))

(use-package smex
  :ensure t
  :init
  (setq smex-save-file (iensu/-config-file ".local/smex-items")))
