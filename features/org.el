;;; Org mode configuration

;;; Code:

;;;; Helper functions and variables

(defun iensu--get-current-inactive-timestamp ()
  (concat "[" (format-time-string "%F %a %H:%M") "]"))

(defun iensu/org-save-buffers ()
  "Saves all org buffers."
  (interactive)
  (let ((before-save-hook '()))
    (save-some-buffers 'no-confirm
                       (lambda ()
                         (string-match-p
                          (expand-file-name org-directory)
                          (buffer-file-name (current-buffer)))))))

(defvar iensu--timer:org-save-buffers nil
  "Org save buffers timer object. Can be used to cancel the timer.")

;; HTTP requests in Org files
(use-package ob-restclient
  :after (org))

;;;; Org package configuration
(use-package org
  :straight (org :type git :repo "https://code.orgmode.org/bzg/org-mode.git" :branch "main")
  :mode (("\\.org\\'" . org-mode)
         ("\\.org.draft\\'" . org-mode))
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c l" . org-store-link)
         :map org-mode-map
         ("H-."   . org-time-stamp-inactive)
         ("C-c s" . org-schedule))
  :hook
  (org-mode . (lambda ()
                (visual-line-mode 1)
                (company-mode -1)))
  :init
  ;; Necessary to make Org-mode stuff available
  (require 'org)
  (require 'org-indent)
  (require 'org-tempo)
  (require 'org-id)
  (require 'ob-js)
  :config
  (setq org-directory iensu-org-dir)
  (setq org-default-notes-file (expand-file-name "notes.org" org-directory))

  (setq org-src-fontify-natively t)
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-fontify-done-headline t)
  (setq org-fontify-whole-heading-line t)

  (setq org-hide-emphasis-markers t)
  (setq org-ellipsis " ▾")

  (setq org-refile-targets `((,(append
                                (directory-files iensu-org-dir :full-path "\\.org$")
                                (directory-files-recursively (concat iensu-org-dir "/projects") "\\.org$"))
                              :maxlevel . 10)))
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-outline-path 'file)

  (setq org-image-actual-width nil)

  (setq org-adapt-indentation nil)
  (setq org-hide-leading-stars t)
  (setq org-indent-indentation-per-level 2)
  (setq org-checkbox-hierarchical-statistics nil)
  (setq org-log-done 'time)
  (setq org-outline-path-complete-in-steps nil)

  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (setq org-id-prefix "org")

  (setq org-export-initial-scope 'subtree)
  (setq org-catch-invisible-edits 'show-and-error)
  (setq org-archive-location "archive/%s_archive::")

  (setq org-modules '(org-protocol))

  (org-load-modules-maybe t)

  (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)
                                                           (shell . t)
                                                           (js . t)
                                                           (C . t)
                                                           (restclient . t)
                                                           (calc . t)))

  (let ((additional-org-templates '(("ssh" . "src shell")
                                    ("sb"  . "src bash")
                                    ("sel" . "src emacs-lisp")
                                    ("sr"  . "src restclient")
                                    ("sa"  . "src artist")
                                    ("st"  . "src typescript"))))
    (dolist (template additional-org-templates)
      (add-to-list 'org-structure-template-alist template)))
  ;; (dolist (face '(org-special-keyword
  ;;                 org-drawer
  ;;                 org-date))
  ;;   (set-face-attribute face nil :height 0.8))
  ;; (set-face-attribute 'org-link nil :underline nil)
  )

;; YAML support in source blocks
(defun org-babel-execute:yaml (body params) body)

;;;; Markdown exporters

;; Standard markdown
(require 'ox-md)

;; Github-flavoured markdown
(use-package ox-gfm
  :init
  (eval-after-load "org"
    '(require 'ox-gfm nil t)))

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
      '((sequence "TODO(t)" "NEXT(n)" "PROJ(p)" "WAITING(w)" "BLOCKED(b)"
                  "|"
                  "CANCELED(C@/!)" "DELEGATED(d)" "POSTPONED(P@/!)" "DONE(D)")))

(defun iensu/gtd-maybe-mark-project-as-done (_n-done _n-not-done)
  "Mark PROJ as done if it has a completion rate of 100%."
  (let* ((headline (org-entry-get nil "ITEM"))
         (state (org-entry-get nil "TODO"))
         (is-proj (string-equal state "PROJ"))
         (mark-as-done (string-match-p "\\[100%\\]" headline)))
    (when (and is-proj mark-as-done)
      (org-todo "DONE"))))

(add-hook 'org-after-todo-statistics-hook 'iensu/gtd-maybe-mark-project-as-done)

;; (setq org-todo-keyword-faces
;;       '(("BLOCKED"   . (:foreground "#dd0066" :weight bold))
;;         ("CANCELED" . (:foreground "#6272a4"))
;;         ("POSTPONED" . (:foreground "#3388ff"))))

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
                   `("T" "TODO with link" entry (file ,(expand-file-name "refile.org" iensu-org-dir))
                     ,(concat "* TODO %?\n"
                              "%a")
                     :empty-lines 1)

                   `("t" "TODO" entry (file ,(expand-file-name "refile.org" iensu-org-dir))
                     ,(concat "* TODO %?\n")
                     :empty-lines 1)

                   `("j" "Journal" entry (file+datetree ,(expand-file-name "journal.org.gpg" iensu-org-dir))
                     ,(concat "* %^{Titel}\n"
                              "%U, %^{Location|Stockholm, Sverige}\n\n"
                              "%?")
                     :empty-lines 1)

                   `("d" "Date entry" entry (file ,(expand-file-name "refile.org" iensu-org-dir))
                     ,(concat "* %<%Y-%m-%d %A>\n"
                              "%t\n"
                              "%?"))

                   `("l" "Link" entry (file ,(expand-file-name "refile.org" iensu-org-dir))
                     ,(concat "* %? %^L %^G \n"
                              "%U")
                     :prepend t)

                   `("L" "Browser Link" entry (file ,(expand-file-name "links.org" iensu-org-dir))
                     ,(concat "* %:description\n"
                              "%:link\n"
                              "%U\n")
                     :prepend t :immediate-finish t :empty-lines 1)

                   `("p" "Browser Link with Selection" entry (file ,(expand-file-name "links.org" iensu-org-dir))
                     ,(concat "* %^{Title}\n"
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

(use-package org-mind-map
  :load-path (lambda () (expand-file-name "packages" user-emacs-directory))
  :init
  (require 'ox-org)
  :config
  ;; (setq org-mind-map-engine "dot")       ; Default. Directed Graph
  (setq org-mind-map-engine "neato")  ; Undirected Spring Graph
  ;; (setq org-mind-map-engine "twopi")  ; Radial Layout
  ;; (setq org-mind-map-engine "fdp")    ; Undirected Spring Force-Directed
  ;; (setq org-mind-map-engine "sfdp")   ; Multiscale version of fdp for the layout of large graphs
  ;; (setq org-mind-map-engine "twopi")  ; Radial layouts
  ;; (setq org-mind-map-engine "circo")  ; Circular Layout
  )

(defun iensu/org-get-anchor-link-friendly-custom-id ()
  "Gets the the `CUSTOM_ID' property of the current org entry or generates an anchor link friendly ID
based on the title."
  (interactive)
  (let ((existing-id (org-entry-get nil "CUSTOM_ID")))
    (if (and existing-id (string-match "\\S+" existing-id))
        existing-id
      (cl-flet ((title->id (title)
                  (let* ((no-subtitle (first (split-string title ":")))
                         (lowercase (downcase no-subtitle))
                         (no-weird-chars (replace-regexp-in-string "[\.\,\+\?\(\)\~\!]+" "" lowercase))
                         (no-whitespace (replace-regexp-in-string "\s+" "-" no-weird-chars)))
                    no-whitespace)))
        (let* ((title (org-entry-get nil "ITEM"))
               (id (title->id title)))
          (org-entry-put nil "CUSTOM_ID" id)
          id)))))

(defun iensu/org-add-anchor-link-friendly-ids-to-headlines-in-file ()
  (interactive)
  (org-map-entries #'iensu/org-get-anchor-link-friendly-custom-id))

(use-package org-tree-slide)
(global-set-key (kbd "<f8>") 'org-tree-slide-mode)
(global-set-key (kbd "S-<f8>") 'org-tree-slide-skip-done-toggle)

(use-package denote
  :straight (:source gnu-elpa-mirror)
  :config
  (setq denote-directory iensu-denote-dir))
