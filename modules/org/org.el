;;; modules/org/org.el --- Org setup

;;; Code:

(defun iensu/open-calendar-buffer ()
  "Opens a calendar buffer."
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list (cfw:org-create-source "Green"))))

(defvar iensu/org-private-file "~/Dropbox/org/private.org")
(defvar iensu/org-work-file "~/Dropbox/org/work.org")
(defvar iensu/org-notes-file "~/Dropbox/org/notes.org")
(defvar iensu/org-refile-file "~/Dropbox/org/refile.org")
(defvar iensu/org-journal-file "~/Dropbox/org/journal.org")
(defvar iensu/org-appointments-file "~/Dropbox/org/appointments.org")

(set-register ?p `(file . ,iensu/org-private-file))
(set-register ?w `(file . ,iensu/org-work-file))
(set-register ?n `(file . ,iensu/org-notes-file))
(set-register ?j `(file . ,iensu/org-journal-file))
(set-register ?r `(file . ,iensu/org-refile-file))
(set-register ?a `(file . ,iensu/org-appointments-file))

(use-package org
  :ensure t
  :defer t
  :init
  (global-set-key (kbd "C-c c") 'org-capture)
  (global-set-key (kbd "C-c a") 'org-agenda)
  :config
  (add-hook 'org-mode-hook (lambda ()
                             (setq org-src-fontify-natively t
                                   org-format-latex-options (plist-put org-format-latex-options :scale 1.5)
                                   truncate-lines t
                                   org-image-actual-width nil
                                   line-spacing 1
                                   org-adapt-indentation nil
                                   org-fontify-quote-and-verse-blocks t
                                   org-fontify-done-headline t
                                   org-fontify-whole-heading-line t
                                   org-hide-leading-stars t
                                   org-indent-indentation-per-level 2
                                   outline-blank-line t)))
  (add-hook 'org-mode-hook (lambda () (add-to-list 'org-src-lang-modes '("javascript" . js2))))
  (add-hook 'org-mode-hook (lambda () (add-to-list 'org-src-lang-modes '("es" . es))))
  (add-hook 'org-mode-hook (lambda () (linum-mode -1)))
  (add-hook 'org-mode-hook (lambda () (auto-fill-mode 1)))
  (add-hook 'org-mode-hook (lambda () (when (executable-find "ispell")
                                   (flyspell-mode 1))))
  (org-babel-do-load-languages
   'org-babel-load-languages '((emacs-lisp . t)
                               (sh . t)
                               (js . t)
                               (python . t)
                               (clojure . t)
                               (elixir . t)))
  (add-to-list 'org-modules 'org-protocol)
  (add-to-list 'org-modules 'ox-md)
  (add-to-list 'org-modules 'org-reveal)
  (org-load-modules-maybe t)
  (require 'org-protocol)
  (setq org-agenda-files (list iensu/org-private-file
                               iensu/org-work-file
                               iensu/org-notes-file
                               iensu/org-refile-file
                               iensu/org-journal-file
                               iensu/org-appointments-file)
        org-default-notes-file iensu/org-notes-file
        org-directory "~/Dropbox/org/"
        org-capture-templates `(("t" "todo" entry (file ,iensu/org-refile-file)
                                 "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
                                ("j" "Journal" entry (file+datetree ,iensu/org-journal-file)
                                 "* %?\n%U\n" :clock-in t :clock-resume t)
                                ("l" "Link" entry (file+headline ,iensu/org-refile-file "Links")
                                 "* %? %^L %^g \n%T" :prepend t)
                                ("L" "Link" entry (file+headline ,iensu/org-refile-file "Links")
                                 "*  %c\n%T" :prepend t :immediate-finish t)
                                ("a" "Appointment" entry (file ,iensu/org-appointments-file)
                                 "* %^{title} %^G \nSCHEDULED: %^T\n\n%?")
                                ("p" "Chrome Note" entry (file+headline ,iensu/org-refile-file "Chrome Notes")
                                 "* %^{Title}\n%T\n\n  Source: %u, %c\n\n  %i"
                                 :prepend t :immediate-finish t))
        org-todo-keywords '((sequence "TODO" "DOING" "|" "DONE"))))

(use-package org-bullets
  :defer t
  :ensure t
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  :config
  (setq org-bullets-bullet-list '("#")))

(use-package ox-reveal
  :defer t
  :ensure t
  :config
  (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/"))

(use-package calfw
  :ensure t
  :defer t
  :init
  (global-set-key (kbd "C-h C-c") 'iensu/open-calendar-buffer))

(use-package calfw-org :ensure t :defer t)
