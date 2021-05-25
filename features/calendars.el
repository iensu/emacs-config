(require 'calendar)

(defvar iensu--diary-file "~/diary")
(defvar iensu--ics-calendars nil)

(setq diary-file iensu--diary-file)
(add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
(add-hook 'diary-mark-entries-hook 'diary-mark-included-diary-files)

(if (boundp 'org-agenda-include-diary)
    (setq org-agenda-include-diary t))

(defun iensu/sync-ics-calendars ()
  (interactive)
  (dolist (calendar iensu--ics-calendars)
    (let (($diary-file (concat "~/diaries/" (car calendar)))
          ($ics-file "/tmp/tmp.ics"))
      (when (file-exists-p $diary-file)
        (delete-file $diary-file))
      (url-copy-file (cdr calendar) $ics-file t)
      (icalendar-import-file $ics-file $diary-file)
      (kill-buffer (find-buffer-visiting $diary-file))
      (kill-buffer (find-buffer-visiting $ics-file))
      (delete-file $ics-file))))
