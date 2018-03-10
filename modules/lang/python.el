;;; modules/lang/python.el --- Python setup

;;; Code:

(add-to-list 'package-archives
             '("elpy" . "https://jorgenschaefer.github.io/packages/"))

(use-package elpy
  :init
  (elpy-enable))
