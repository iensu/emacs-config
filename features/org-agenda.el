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
                         (org-agenda-max-entries 10)))
             (agenda ""
                     ((org-agenda-start-day "0d")
                      (org-agenda-span 1)
                      (org-agenda-start-on-weekday nil)))))))
  (add-to-list 'org-agenda-custom-commands agenda-command))

(setq org-agenda-files (directory-files iensu-org-dir :full-path "projects.org$")
      org-agenda-dim-blocked-tasks nil
      org-deadline-warning-days -7
      org-agenda-block-separator "")

(plist-put org-agenda-clockreport-parameter-plist :maxlevel 6)

;; Clean-up agenda view
(setq org-agenda-prefix-format
      '((agenda . "   %?-12t    % s")
        (todo . " %i %-12:c")
        (tags . " %i %-12:c")
        (search . " %i %-12:c")))

;; Update the calendar to contain Swedish holidays etc.
(load-file (expand-file-name "packages/kalender.el" user-emacs-directory))
