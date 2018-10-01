;;; modules/core/git.el --- Git

;;; Code:

(use-package magit
  :delight
  (magit-status-mode "魔" :major)
  :bind (("C-x g" . magit-status)))

(use-package git-gutter
  :diminish
  :init
  (global-git-gutter-mode 1)
  (git-gutter))

(use-package git-timemachine)
