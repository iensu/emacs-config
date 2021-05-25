;; Stores google calendar events to my org `work-calendar' file. Sync by running `M-x org-gcal-sync'.
(use-package org-gcal
  :init
  (setq org-gcal-token-file (expand-file-name ".local/org-gcal/org-gcal-token" user-emacs-directory)
        org-gcal-dir (expand-file-name ".local/org-gcal/" user-emacs-directory))

  :config
  (setq org-gcal-client-id iensu-gcal-client-id
        org-gcal-client-secret iensu-gcal-client-secret

        org-gcal-down-days 30
        org-gcal-up-days 30
        org-gcal-remove-api-cancelled-events t

        ;; https://github.com/kidd/org-gcal.el/issues/107
        org-id-locations-file (expand-file-name ".local/.org-id-locations" user-emacs-directory))

  (let ((work-calendar (expand-file-name "calendars/work.org" org-directory)))
    (setq org-gcal-file-alist `(("jens.ostlund@futurice.com" . ,work-calendar)))
    (add-to-list 'org-agenda-files work-calendar)))

(defun iensu/refresh-work-calendar ()
  "Fetch Google calendar events and add the proper file tag(s)."
  (interactive)
  (org-gcal-fetch))

(defvar iensu--timer:update-work-calendar nil)

(defun iensu/start-work-calendar-update-timer ()
  (interactive)
  (setq iensu--timer:update-work-calendar
        (run-at-time t (* 30 60) #'iensu/refresh-work-calendar)))

(defun iensu/stop-work-calendar-timer ()
  (interactive)
  (cancel-timer iensu--timer:update-work-calendar))
