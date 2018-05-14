;; modules/core/navigation.el --- Navigation

;;; Code:

(use-package dired+
  :load-path (lambda () (iensu--config-file "packages"))
  :config
  (when (executable-find "gls") ;; native OSX ls works differently then GNU ls
    (setq insert-directory-program "/usr/local/bin/gls"))
  (setq dired-listing-switches "-alGh --group-directories-first"
        dired-dwim-target t))

(use-package dired-subtree
  :ensure t
  :config
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)
             (";" . dired-subtree-remove)))

(use-package neotree
  :config
  (global-set-key [f8] 'neotree-toggle)
  (setq neo-theme 'icons))

(use-package projectile
  :delight '(:eval (let ((project-name (projectile-project-name)))
                     (if (string-equal project-name "-")
                         ""
                       (concat " <" project-name ">"))))
  :init
  (setq projectile-cache-file (iensu--config-file ".local/projectile.cache")
        projectile-known-projects-file (iensu--config-file ".local/projectile-bookmarks.eld"))
  :config
  (projectile-global-mode)
  (add-hook 'projectile-after-switch-project-hook 'iensu/use-local-eslint)
  (setq projectile-sort-order 'access-time)
  (let ((ignored-files '(".DS_Store" ".projectile")))
    (dolist (file ignored-files)
      (add-to-list 'projectile-globally-ignored-files file))))

(use-package recentf
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 20))

(use-package sr-speedbar
  :bind (("H-s" . sr-speedbar-toggle))
  :config
  (setq sr-speedbar-default-width 60)
  (speedbar-add-supported-extension ".elm"))

(use-package which-key
  :delight
  :config
  (which-key-mode))

(use-package counsel
  :delight ivy-mode
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
        ivy-use-selectable-prompt t
        ivy-count-format "(%d/%d) "
        ivy-magic-slash-non-match-action 'ivy-magic-non-match-create))

(use-package counsel-projectile :init (counsel-projectile-mode 1))

(use-package ido
  :init
  (ido-mode t)
  :config
  (setq ido-everywhere t
        ido-enable-flex-matching t
        ido-use-filename-at-point 'guess
        ido-create-new-buffer 'always
        ido-save-directory-list-file (iensu--config-file ".local/ido.last")))

(use-package smex
  :init
  (setq smex-save-file (iensu--config-file ".local/smex-items")))
