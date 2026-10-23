;; -*- lexical-binding: t; -*-

(use-package pdf-tools
  :ensure t
  :vc (pdf-tools :url "https://github.com/vedang/pdf-tools"
                 :rev "5245f092e35712df6559a7782a93bb61896175dd")
  :init
  (pdf-tools-install))
