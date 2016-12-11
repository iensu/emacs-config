;;; iensu --- Some utility functions

;;; Commentary:

;;; Code:

(defun iensu/add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'.  Taken from `hrs/add-auto-mode'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

(defun iensu/duplicate-line (n)
  "Copy the current line N times and insert it below."
  (interactive "P")
  (let ((cur-pos (point)))
    (dotimes (i (prefix-numeric-value n))
      (move-beginning-of-line nil)
      (kill-line)
      (yank)
      (newline)
      (insert (s-trim (car kill-ring)))
      (goto-char cur-pos))))

(defun iensu/pick-nodejs-version ()
  (let ((most-recent (car (last (sort (nvm--installed-versions)
                                      (lambda (a b) (string-lessp (car a) (car b)))))))
        (nvmrc? (lambda () (file-exists-p (concat (projectile-project-root) ".nvmrc")))))
    (cond ((not (projectile-project-p)) (nvm-use most-recent))
          ((not (funcall nvmrc?)) (nvm-use most-recent))
          (t (nvm-use-for (projectile-project-root))))))

(defun iensu/setup-line-numbers ()
  (linum-mode 1)
  (setq linum-format "%3d ")
  (set-face-attribute 'linum nil :height 120)
  (column-number-mode 1))

(defun iensu/tern-restart-server ()
  "Restart the tern server."
  (interactive)
  (delete-process "Tern"))

(defun iensu/use-local-eslint ()
  "Try to use local eslint executable from node_modules."
  (interactive)
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (global-eslint (executable-find "eslint"))
         (local-eslint (expand-file-name "node_modules/.bin/eslint"
                                         root))
         (eslint (if (file-executable-p local-eslint)
                     local-eslint
                   global-eslint)))
    (setq-local flycheck-javascript-eslint-executable eslint)))

(defun iensu/toggle-scratch-buffer ()
  "Based on a great idea from Eric Skoglund (https://github.com/EricIO/emacs-configuration/)."
  (interactive)
  (if (string-equal (buffer-name (current-buffer))
                    "*scratch*")
      (switch-to-buffer (other-buffer))
    (switch-to-buffer "*scratch*")))

(defun iensu/buffer-content-string (buffer)
  "Return `BUFFER' content as a string."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun iensu/--node-debugger-url (shell-output)
  "Return the Node.js debugger url contained in `SHELL-OUTPUT' if any."
  (car (remove-if-not (lambda (s) (string-match "^chrome-devtools:.*" s))
                      (mapcar 'string-trim (split-string shell-output "\n")))))

(defun iensu/mocha-debug-file ()
  "Start a Mocha debugging session and copy debugger url to clipboard."
  (interactive)
  (let* ((options '("--inspect" "--debug-brk"))
         (file-path (buffer-file-name))
         (project-root (projectile-project-root))
         (mocha-executable (concat project-root "node_modules/.bin/mocha"))
         (buffer-name "*<Mocha Test Debugger>*")
         (cd-cmd (string-join (list "cd" project-root) " "))
         (mocha-cmd (string-join (list mocha-executable
                                       (string-join options " ")
                                       file-path)
                                 " ")))
    (if (not (file-exists-p mocha-executable))
        (message (concat "Cannot find Mocha executable at " mocha-executable))
      (progn
        (pop-to-buffer (get-buffer-create buffer-name))
        (start-process-shell-command "*Mocha Debugger*"
                                     (get-buffer buffer-name)
                                     (concat cd-cmd " && " mocha-cmd))
        (sleep-for 1)
        (kill-new (iensu/--node-debugger-url (iensu/buffer-content-string (get-buffer buffer-name))))
        (with-current-buffer buffer-name
          (save-excursion
            (goto-char (point-max))
            (insert "\n\nDebugger URL copied to clipboard, please paste in your browser.\n\n")))))))

(defun iensu/mocha-kill-debugger ()
  "Kill running mocha-executable debugger process."
  (interactive)
  (kill-buffer "*Async Shell Command*"))

(defun iensu/open-calendar-buffer ()
  "Opens a calendar buffer."
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list (cfw:org-create-source "Green"))))

(defun iensu/move-file (new-location)
  "Write this file to NEW-LOCATION, and delete the old one.  Copied from http://zck.me/emacs-move-file."
  (interactive (list (if buffer-file-name
                         (read-file-name "Move file to: ")
                       (read-file-name "Move file to: "
                                       default-directory
                                       (expand-file-name (file-name-nondirectory (buffer-name))
                                                         default-directory)))))
  (when (file-exists-p new-location)
    (delete-file new-location))
  (let ((old-location (buffer-file-name)))
    (write-file new-location t)
    (when (and old-location
               (file-exists-p new-location)
               (not (string-equal old-location new-location)))
      (delete-file old-location))))

(provide 'iensu)
;;; iensu.el ends here
