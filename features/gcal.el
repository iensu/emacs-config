;; Stores google calendar events to my org `work-calendar' file. Sync by running `M-x org-gcal-sync'.
(use-package org-gcal
  :init
  (setq org-gcal-token-file (expand-file-name ".local/org-gcal/org-gcal-token" user-emacs-directory)
        org-gcal-dir (expand-file-name ".local/org-gcal/" user-emacs-directory))
  :config
  (setq org-gcal-client-id iensu-gcal-client-id
        org-gcal-client-secret iensu-gcal-client-secret
        org-gcal-file-alist `(("jens.ostlund@futurice.com" . ,(expand-file-name "calendars/work.org"
                                                                                org-directory)))))

(defun iensu/refresh-work-calendar ()
  "Fetch Google calendar events and add the proper file tag(s)."
  (interactive)
  (org-gcal-fetch))
