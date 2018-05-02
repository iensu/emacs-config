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
    (books            ,(concat iensu-org-dir "/books.org")             ?b)
    (work-calendar    ,(concat iensu-org-dir "/calendars/work.org")    ?c)
    (private-calendar ,(concat iensu-org-dir "/calendars/private.org") ?C)
    (finances         ,(concat iensu-org-dir "/finances.org")          ?f)
    (journal          ,(concat iensu-org-dir "/journal.org.gpg")       ?j)
    (notes            ,(concat iensu-org-dir "/notes.org")             ?n)
    (private          ,(concat iensu-org-dir "/private.org")           ?p)
    (projects         ,(concat iensu-org-dir "/projects.org")          ?P)
    (refile           ,(concat iensu-org-dir "/refile.org")            ?r)
    (beorg-refile     ,(concat iensu-org-dir "/refile-beorg.org")      ?R)
    (work             ,(concat iensu-org-dir "/work.org")              ?w)))

(defun iensu--org-remove-file-if-match (&rest regexes)
  (let ((regex (string-join regexes "\\|")))
    (cl-remove-if (lambda (file) (string-match regex file))
                  (mapcar 'cadr iensu-org-files-alist))))

(defvar iensu-org-refile-targets
  (iensu--org-remove-file-if-match "calendars"
                                   "journal"
                                   "appointments"
                                   "refile"))

(defvar iensu-org-agenda-files
  (iensu--org-remove-file-if-match "\.org\.gpg"))

(setq org-outline-path-complete-in-steps t)

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
     ,(concat "* %^{Location|Stockholm, Sweden}\n"
              "%U\n\n"
              "%?\n"))

    ("l" "Link" entry (file ,(iensu-org-file 'refile))
     ,(concat "* %? %^L %^G \n"
              "%U\n")
     :prepend t)

    ("L" "Browser Link" entry (file ,(iensu-org-file 'refile))
     ,(concat "*  %a\n"
              "%U\n")
     :prepend t :immediate-finish t)

    ("p" "Browser Link and Selection" entry (file ,(iensu-org-file 'refile))
     ,(concat "* %^{Title}\n"
              "Source: %u, %c\n"
              "#+BEGIN_QUOTE\n"
              "%i\n"
              "#+END_QUOTE\n\n\n%?")
     :prepend t)

    ("a" "Appointment" entry (file ,(iensu-org-file 'appointments))
     ,(concat "* %^{title} %^G\n"
              "SCHEDULED: %^T\n\n"
              "%?\n"))

    ("n" "Notes" entry (file+headline ,(iensu-org-file 'notes) "Notes")
     ,(concat "* %^{Title} %^G\n"
              "%U\n\n"
              "%?\n"))
    ("b" "Book" entry (file+headline ,(iensu-org-file 'books) "Läslista")
     ,(concat "* %^{STATE|TO_READ|FINISHED} %^{} <%^{}> %^g\n\n"))))

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
        outline-blank-line t
        org-adapt-indentation nil
        org-fontify-quote-and-verse-blocks t
        org-fontify-done-headline t
        org-fontify-whole-heading-line t
        org-hide-leading-stars t
        org-indent-indentation-per-level 2
        org-checkbox-hierarchical-statistics nil
        org-log-done 'time)
  (when (or (executable-find "ispell")
            (executable-find "aspell"))
    (flyspell-mode 1)
    (when (executable-find "aspell")
      (setq ispell-program-name "aspell"
            ispell-extra-args '("--sug-mode=ultra")))))

(use-package org
  :delight
  (org-mode "\u2658" :major)
  :init
  (global-set-key (kbd "C-c c") 'org-capture)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (require 'ox-md)
  :config
  (add-hook 'org-mode-hook 'iensu--org-mode-hook)
  (org-babel-do-load-languages
   'org-babel-load-languages '((emacs-lisp . t)
                               (sh . t)
                               (js . t)
                               (python . t)
                               (clojure . t)
                               (elixir . t)))
  (org-load-modules-maybe t)
  (require 'org-protocol)
  (setq org-agenda-files iensu-org-agenda-files
        org-default-notes-file (iensu-org-file 'notes)
        org-directory iensu-org-dir
        org-capture-templates iensu-org-capture-templates-alist
        org-todo-keywords '((sequence "TODO" "DOING" "|" "DONE"))
        org-refile-targets '((iensu-org-refile-targets :maxlevel . 3))
        org-deadline-warning-days -7
        ;; org-agenda optimizations
        org-agenda-dim-blocked-tasks nil))

(use-package org-bullets
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  :config
  (setq org-bullets-bullet-list '("*")))

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
