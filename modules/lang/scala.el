;;; lang/scala.js --- Scala setup

(use-package scala-mode :defer t :ensure t)

(use-package sbt-mode :defer t :ensure t)

(use-package ensime
  :ensure t
  :defer t
  :config
  (add-hook 'ensime-mode-hook (lambda ()
                                (add-to-list 'company-backends 'ensime-company))))
