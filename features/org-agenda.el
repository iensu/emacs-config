(require 'org-agenda)

(dolist (agenda-command
         '(("z" "One week agenda"
            ((todo "TODO|STARTED"
                        ((org-agenda-overriding-header "TODOs")
                         (org-agenda-prefix-format "  ")
                         (org-agenda-sorting-strategy '(priority-down deadline-up))
                         (org-agenda-max-entries 20)))
             (agenda ""
                     ((org-agenda-start-day "0d")
                      (org-agenda-span 1)
                      (org-agenda-start-on-weekday nil)))))))
  (add-to-list 'org-agenda-custom-commands agenda-command))

(setopt org-agenda-files (directory-files "~/Nextcloud/notes/log" :full ".+.org$")
        org-agenda-dim-blocked-tasks nil
        org-deadline-warning-days -5
        org-agenda-block-separator "")

(plist-put org-agenda-clockreport-parameter-plist :maxlevel 6)

;; Clean-up agenda view
(setopt org-agenda-prefix-format
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

(defun iensu/open-project-org-file ()
  (interactive)
  (find-file (iensu--org-capture-project-notes-file)))

(defun iensu/refresh-agenda-files ()
  (interactive)
  (setopt org-agenda-files (directory-files "~/Nextcloud/notes/log" :full ".+.org$")))
