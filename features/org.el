;;;; Helper functions and variables
(defun iensu--get-current-inactive-timestamp ()
  (concat "[" (format-time-string "%F %a %H:%M") "]"))

(defun iensu/org-save-buffers ()
  "Saves all org buffers."
  (interactive)
  (save-some-buffers 'no-confirm
                     (lambda ()
                       (string-match-p
                        (expand-file-name org-directory)
                        (buffer-file-name (current-buffer))))))

(defvar iensu--timer:org-save-buffers nil
  "Org save buffers timer object. Can be used to cancel the timer.")

(use-package org
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c l" . org-store-link)
         :map org-mode-map
         ("H-."   . org-time-stamp-inactive))
  :hook
  (org-mode . (lambda ()
                (org-num-mode 1)
                (visual-line-mode 1)
                (variable-pitch-mode 1)))
  :init
  ;; Necessary to make Org-mode stuff available
  (require 'org)
  :config
  (setq org-directory iensu-org-dir)
  (setq org-default-notes-file (expand-file-name "notes.org" org-directory))
  (setq org-refile-targets '((iensu-org-refile-targets :maxlevel . 10)))
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-outline-path 'file)
  (setq org-latex-listings t)
  (setq org-cycle-separator-lines 1)
  (setq org-src-fontify-natively t)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
  (setq truncate-lines t)
  (setq org-image-actual-width nil)
  (setq line-spacing 1)
  (setq outline-blank-line t)
  (setq org-adapt-indentation nil)
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-fontify-done-headline t)
  (setq org-fontify-whole-heading-line t)
  (setq org-hide-leading-stars t)
  (setq org-indent-indentation-per-level 2)
  (setq org-checkbox-hierarchical-statistics nil)
  (setq org-log-done 'time)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-html-htmlize-output-type 'css)
  (setq org-export-initial-scope 'subtree)
  (setq org-catch-invisible-edits 'show-and-error)
  (setq org-archive-location "archive/%s_archive::")

  (setq org-modules '(org-protocol))

  (org-load-modules-maybe t)
  (dolist (lang-mode '(("javascript" . js2) ("es" . es) ("wat" . wat))
                     (add-to-list 'org-src-lang-modes lang-mode)))

  (org-babel-do-load-languages
   'org-babel-load-languages '((emacs-lisp . t)
                               (shell . t)
                               (js . t)
                               (python . t)
                               (dot . t)))

  (let ((additional-org-templates (if (version< (org-version) "9.2")
                                    '(("ssh" "#+begin_src shell \n?\")
                                        ("sel" " \n?\"))
                                  '(("ssh" . "src shell")
                                    ("sel" . "src emacs-lisp")
                                    ("sr"  . "src restclient")
                                    ("sR"  .  "src rust")))))
    (dolist (template additional-org-templates)
      (add-to-list 'org-structure-template-alist template))))

(setq calendar-week-start-day 1) ; The week starts on Monday.

;;;; Markdown export
;; standard markdown
(require 'ox-md)

;; Github-flavoured markdown
(use-package ox-gfm
  :init
  (eval-after-load "org"
    '(require 'ox-gfm nil t)))

;;;; YAML support in source blocks
(defun org-babel-execute:yaml (body params) body)

;;;; Capture things everywhere

;;`org-protocol' enables capturing from outside of Emacs.
(require 'org-protocol)

(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame"
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

(defadvice org-capture-destroy
    (after delete-capture-frame activate)
  "Advise capture-destroy to close the frame"
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

;;;; TODO keyword and priorities setup
(setq org-todo-keywords
      '((sequence "TODO(t)" "DOING(d!)" "BLOCKED(b@/!)"
                  "|"
                  "CANCELED(C@/!)" "POSTPONED(P@/!)" "DONE(D@/!)")))

(setq org-todo-keyword-faces
      '(("BLOCKED"   . (:foreground "#dd0066" :weight bold))
        ("CANCELED" . (:foreground "#6272a4"))
        ("POSTPONED" . (:foreground "#3388ff"))))

;; Customize PRIORITIES
(setq org-highest-priority ?A
      org-default-priority ?D
      org-lowest-priority  ?E)

(setq org-clock-in-switch-to-state "DOING")
(setq org-log-into-drawer t)


;;;;; Make org-mode prettier

;; Make view more compact
(setq org-cycle-separator-lines 0)

;; Only display one bullet per headline for a cleaner look.
(use-package org-superstar
  :after (org)
  :init
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
  :config
  (setq org-superstar-headline-bullets-list '(?◉)))

;;;; Autosaving org buffers
(setq iensu--timer:org-save-buffers
      (run-at-time t (* 5 60) #'iensu/org-save-buffers))

;;;; Capture templates
(iensu-add-to-list 'iensu-org-capture-templates
                   `("t" "TODO with link" entry (file ,(expand-file-name "refile.org" iensu-org-dir))
                     ,(concat "* TODO %?\n"
                              "%U\n"
                              "%a")
                     :empty-lines 1)

                   `("T" "TODO" entry (file ,(expand-file-name "refile.org" iensu-org-dir))
                     ,(concat "* TODO %?\n"
                              "%U")
                     :empty-lines 1)

                   `("j" "Journal" entry (file+datetree ,(expand-file-name "journal.org.gpg" iensu-org-dir))
                     ,(concat "* %^{Titel}\n"
                              "%U, %^{Location|Stockholm, Sverige}\n\n"
                              "%?")
                     :empty-lines 1)

                   `("l" "Link" entry (file ,(expand-file-name "refile.org" iensu-org-dir))
                     ,(concat "* %? %^L %^G \n"
                              "%U")
                     :prepend t)

                   `("L" "Browser Link" entry (file ,(expand-file-name "refile.org" iensu-org-dir))
                     ,(concat "* TODO %:description\n"
                              "%U\n\n"
                              "%:link")
                     :prepend t :immediate-finish t :empty-lines 1)

                   `("p" "Browser Link and Selection" entry (file ,(expand-file-name "refile.org" iensu-org-dir))
                     ,(concat "* TODO %^{Title}\n"
                              "Source: %u, %c\n\n"
                              "#+BEGIN_QUOTE\n"
                              "%i\n"
                              "#+END_QUOTE\n\n\n%?")
                     :prepend t :empty-lines 1)

                   `("b" "Book" entry (file+headline ,(expand-file-name "private.org" iensu-org-dir) "Reading list")
                     ,(concat "* %^{Title}"
                              " %^{Author}p"
                              " %^{Genre}p"
                              " %^{Published}p"
                              " %(org-set-property \"Added\" (iensu--get-current-inactive-timestamp))")
                     :prepend t :empty-lines 1))

(setq org-capture-templates iensu-org-capture-templates)

(pretty-hydra-define+ iensu-hydra ()
  ("Org clock"
   (("c c" org-clock-in      "start clock")
    ("c r" org-clock-in-last "resume clock")
    ("c s" org-clock-out     "stop clock")
    ("c g" org-clock-goto    "goto clocked task"))))
