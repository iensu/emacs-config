(require 'org-agenda)

(setq iensu-org-agenda-files
      (iensu--org-remove-file-if-match "\\.org\\.gpg"))

(dolist (agenda-command
         '(("z" "Two week agenda"
            ((tags-todo "-books-music-movies"
                        ((org-agenda-overriding-header "TODOs")
                         (org-agenda-prefix-format "  ")
                         (org-agenda-sorting-strategy '(priority-down deadline-up))
                         (org-agenda-max-entries 20)))
             (agenda ""
                     ((org-agenda-start-day "0d")
                      (org-agenda-span 14)
                      (org-agenda-start-on-weekday nil)))))))
  (add-to-list 'org-agenda-custom-commands agenda-command))

(setq org-agenda-files iensu-org-agenda-files
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
(load-file (iensu--config-file "packages/kalender.el"))
