(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)

(unless (package-installed-p 'pallet)
  (package-install 'pallet))

(require 'pallet)
(pallet-mode t)

(require 'use-package)

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))
