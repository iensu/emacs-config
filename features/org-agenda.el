(require 'org-agenda)

(dolist (agenda-command
         '(("z" "One week agenda"
            ((tags-todo "+TODO=\"NEXT\""
                        ((org-agenda-overriding-header "Next actions")
                         (org-agenda-prefix-format "  ")
                         (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("PROJ") 'regexp ":(books|music|movies|refile|links):"))
                         (org-agenda-sorting-strategy '(priority-down deadline-up))
                         (org-agenda-max-entries 20)))
             (tags-todo "+TODO=\"PROJ\""
                        ((org-agenda-overriding-header "Active projects")
                         (org-agenda-prefix-format "  ")
                         (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp ":recurring:"))
                         (org-agenda-max-entries 10)))
             (agenda ""
                     ((org-agenda-start-day "0d")
                      (org-agenda-span 1)
                      (org-agenda-start-on-weekday nil)))))))
  (add-to-list 'org-agenda-custom-commands agenda-command))

(setq org-agenda-files (cl-loop for f in '("projects.org")
                                collect (expand-file-name f iensu-org-dir))
      org-agenda-dim-blocked-tasks nil
      org-deadline-warning-days -3
      org-agenda-block-separator "")

(plist-put org-agenda-clockreport-parameter-plist :maxlevel 6)

;; Clean-up agenda view
(setq org-agenda-prefix-format
      '((agenda . "   %?-12t    % s")
        (todo . " %i %-12:c")
        (tags . " %i %-12:c")
        (search . " %i %-12:c")))

;; https://christiantietze.de/posts/2022/12/manage-org-agenda-related-buffers-via-display-buffer-alist/
(add-to-list 'display-buffer-alist
             '("\\*Org Agenda\\*"
               (display-buffer-in-tab display-buffer-reuse-mode-window)
               (ignore-current-tab . t)
               (tab-name . "Agenda")
               (window-height . .8)
               (dedicated . t)
               (inhibit-same-window . nil)))

;; Update the calendar to contain Swedish holidays etc.
(load-file (expand-file-name "packages/kalender.el" user-emacs-directory))

;;;;; Project-based TODO lists

;; Create a TODO list based on TODO items in a project's `.project-notes.org' file. The
;; `org-agenda-files' variable is temporarily set the only the project notes file and then reverted
;; back to its previous value upon closing the TODO list buffer.

(defvar iensu--project-agenda-buffer-name "*Project Agenda*")

(defun iensu--org-capture-project-notes-file ()
  (concat (locate-dominating-file (buffer-file-name) ".git") ".project-notes.org"))

(defun iensu/create-project-notes-file ()
  "Creates a note file somewhere in `org-directory' and links it to the current directory as `.project-notes.org.'"
  (interactive)
  (let* ((versioned-dir (locate-dominating-file (buffer-file-name)
                                                ".git"))
         (project-dir (expand-file-name (or versioned-dir
                                            (file-name-directory (buffer-file-name)))))
         (project-name (car (last (remove-if (lambda (x) (string-equal ""
                                                                       x))
                                             (split-string project-dir
                                                           "/")))))
         (notes-link (concat project-dir ".project-notes.org"))
         (notes-file-name (concat project-name ".org"))
         (notes-dir (expand-file-name (read-directory-name (format "Where to save %s? "
                                                                   notes-file-name)
                                                           (concat (file-name-as-directory org-directory)
                                                                   "projects"))))
         (notes-file (concat notes-dir notes-file-name)))
    (make-empty-file notes-file)
    (make-symbolic-link notes-file notes-link)
    (find-file notes-link)
    (iensu/refresh-agenda-files)))

(defun iensu/refresh-agenda-files ()
  (interactive)
  (setq-default org-agenda-files
                (append (directory-files iensu-org-dir
                                         :full-path
                                         "\\.org$")
                        (directory-files-recursively (concat iensu-org-dir "/projects")
                                                     "\\.org$")
                        (directory-files-recursively (concat iensu-org-dir "/calendars")
                                                     "\\.org$"))))

(defun iensu/project-todo-list ()
  (interactive)
  (let ((project-notes-file (expand-file-name ".project-notes.org"
                                              (project-root (project-current)))))
    (if (file-exists-p project-notes-file)
        (progn
          (setq org-agenda-files `(,project-notes-file))
          (org-todo-list)
          (rename-buffer iensu--project-agenda-buffer-name 'unique))
      (message "Could not locate any project notes file"))))

(defun iensu/reset-org-agenda-files ()
  (interactive)
  (when (string-equal iensu--project-agenda-buffer-name
                      (buffer-name (current-buffer)))
    (setq org-agenda-files iensu-org-agenda-files)))

(defun iensu/open-project-org-file ()
  (interactive)
  (find-file (iensu--org-capture-project-notes-file)))

;; Reset org-agenda-files when the project TODO list buffer is closed
(add-hook 'kill-buffer-hook #'iensu/reset-org-agenda-files)
