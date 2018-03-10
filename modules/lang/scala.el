;;; lang/scala.js --- Scala setup

(use-package scala-mode)

(use-package sbt-mode)

(use-package ensime
  :config
  (add-hook 'ensime-mode-hook (lambda ()
                                (add-to-list 'company-backends 'ensime-company))))
