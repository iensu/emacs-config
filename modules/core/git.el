;;; modules/core/git.el --- Git

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :init
  (global-git-gutter-mode +1)
  (git-gutter:linum-setup))

(use-package git-timemachine :ensure t)
