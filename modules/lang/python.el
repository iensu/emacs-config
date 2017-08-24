;;; lang/python.el --- Python setup

(add-to-list 'package-archives
             '("elpy" . "https://jorgenschaefer.github.io/packages/"))

(use-package elpy
  :defer t
  :ensure t
  :init
  (elpy-enable))
