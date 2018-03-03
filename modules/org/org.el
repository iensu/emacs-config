;;; modules/org/org.el --- Org setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defun iensu/open-calendar-buffer ()
  "Opens a calendar buffer."
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list (cfw:org-create-source "Green"))))

(defvar iensu-org-dir "~/Dropbox/org")

(defvar iensu-org-files-alist
  `((appointments     ,(concat iensu-org-dir "/appointments.org")      ?a)
    (work-calendar    ,(concat iensu-org-dir "/calendars/work.org")    ?c)
    (private-calendar ,(concat iensu-org-dir "/calendars/private.org") ?C)
    (finances         ,(concat iensu-org-dir "/finances.org")          ?f)
    (journal          ,(concat iensu-org-dir "/journal.org")           ?j)
    (notes            ,(concat iensu-org-dir "/notes.org")             ?n)
    (private          ,(concat iensu-org-dir "/private.org")           ?p)
    (refile           ,(concat iensu-org-dir "/refile.org")            ?r)
    (work             ,(concat iensu-org-dir "/work.org")              ?w)))

(defvar iensu-org-refile-targets
  (let ((filepaths (mapcar 'cadr iensu-org-files-alist)))
    (cl-remove-if (lambda (fp)
                    (string-match "calendars" fp))
                  filepaths)))

;; Add register shortcuts for all org files
(dolist (file-props iensu-org-files-alist)
  (set-register (car (cddr file-props)) `(file . ,(cadr file-props))))

(defun iensu-org-file (key)
  "Return file path for org file matching KEY. KEY must be in `iensu-org-files-alist'."
  (cadr (assoc key iensu-org-files-alist)))

(defvar iensu-org-capture-templates-alist
  `(("t" "TODO" entry (file ,(iensu-org-file 'refile))
     ,(concat "* TODO %?\n"
              "%U\n"
              "%a\n")
     :clock-in t :clock-resume t)

    ("j" "Journal" entry (file+datetree ,(iensu-org-file 'journal))
     ,(concat "* %?\n"
              "%U\n")
     :clock-in t :clock-resume t)

    ("l" "Link" entry (file ,(iensu-org-file 'refile))
     ,(concat "* %? %^L %^G \n"
              "%U\n")
     :prepend t)

    ("b" "Browser Link" entry (file ,(iensu-org-file 'refile))
     ,(concat "*  %a\n"
              "%U\n")
     :prepend t :immediate-finish t)

    ("a" "Appointment" entry (file ,(iensu-org-file 'appointments))
     ,(concat "* %^{title} %^G\n"
              "SCHEDULED: %^T\n\n"
              "%?\n"))

    ("n" "Notes" entry (file+headline ,(iensu-org-file 'notes) "Notes")
     ,(concat "* %^{Title} %^G\n"
              "%U\n\n"
              "%?\n"))))

(defun iensu--org-mode-hook ()
  (define-key org-mode-map (kbd "H-.") 'org-time-stamp-inactive)
  (iensu-add-to-list 'org-src-lang-modes '(("javascript" . js2)
                                           ("es" . es)))
  (linum-mode -1)
  (auto-fill-mode 1)
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
        outline-blank-line t)
  (when (or (executable-find "ispell")
            (executable-find "aspell"))
    (flyspell-mode 1)))

(use-package org
  :delight
  (org-mode "\u2658" :major)
  :init
  (global-set-key (kbd "C-c c") 'org-capture)
  (global-set-key (kbd "C-c a") 'org-agenda)
  :config
  (add-hook 'org-mode-hook 'iensu--org-mode-hook)
  (org-babel-do-load-languages
   'org-babel-load-languages '((emacs-lisp . t)
                               (sh . t)
                               (js . t)
                               (python . t)
                               (clojure . t)
                               (elixir . t)))
  (iensu-add-to-list 'org-modules '(org-protocol ox-md org-reveal))
  (org-load-modules-maybe t)
  (require 'org-protocol)
  (setq org-agenda-files (mapcar (lambda (fprops) (cadr fprops)) iensu-org-files-alist)
        org-default-notes-file (iensu-org-file 'notes)
        org-directory iensu-org-dir
        org-capture-templates iensu-org-capture-templates-alist
        org-todo-keywords '((sequence "TODO" "DOING" "|" "DONE"))
        org-refile-targets '((iensu-org-refile-targets :maxlevel . 3))))

(use-package org-bullets
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  :config
  (setq org-bullets-bullet-list '("#")))

(use-package ox-reveal
  :config
  (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/"))

(use-package calfw
  :init
  (global-set-key (kbd "C-h C-c") 'iensu/open-calendar-buffer))

(use-package calfw-org)

(use-package org-gcal
  :init
  (setq org-gcal-token-file (iensu--config-file ".local/org-gcal/org-gcal-token")
        org-gcal-dir (iensu--config-file ".local/org-gcal/"))
  :config
  (setq org-gcal-client-id *user-gcal-client-id*
        org-gcal-client-secret *user-gcal-client-secret*
        org-gcal-file-alist `(("jens.ostlund@futurice.com" . ,(iensu-org-file 'work-calendar))
                              ("jostlund@gmail.com" . ,(iensu-org-file 'private-calendar)))))
