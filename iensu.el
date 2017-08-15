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
      (insert (s-trim-right (car kill-ring)))
      (goto-char cur-pos))))

(defun iensu/pick-nodejs-version ()
  (let ((most-recent (caar (last (nvm--installed-versions))))
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

(defun iensu/node-project-root ()
  (locate-dominating-file (or (buffer-file-name) default-directory)
                          "node_modules"))

(defun iensu/node-find-local-executable (executable-name)
  (expand-file-name (concat "node_modules/.bin/" executable-name)
                    (iensu/node-project-root)))

(defun iensu/use-local-flow ()
  "Try to use local flow executable from node_modules."
  (interactive)
  (let ((flow (iensu/node-find-local-executable "flow")))
    (when (and flow
               (file-executable-p flow))
      (setq-local flycheck-javascript-flow-executable flow))))

(defun iensu/use-local-eslint ()
  "Try to use local eslint executable from node_modules."
  (interactive)
  (let ((global-eslint (executable-find "eslint"))
        (local-eslint (iensu/node-find-local-executable "eslint")))
    (setq-local flycheck-javascript-eslint-executable
                (if (file-executable-p local-eslint) local-eslint global-eslint))))

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

(defun iensu/switch-to-minibuffer ()
  "Switch to minibuffer window (if active)."
  (interactive)
  (when (active-minibuffer-window)
    (select-frame-set-input-focus (window-frame (active-minibuffer-window)))
    (select-window (active-minibuffer-window))))

(defun iensu/open-eshell-here ()
  "Open an eShell prompt in the project directory.  Inspired by this example: http://www.howardism.org/Technical/Emacs/eshell-fun.html."
  (interactive)
  (let* ((parent (or (ignore-errors (projectile-project-root))
                     default-directory))
         (height (/ (window-total-height) 3))
         (name (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (insert (concat "cd " parent))
    (eshell-send-input)
    (rename-buffer (concat "*eshell: " name "*"))))

(defun iensu/close-eshell ()
  "Close an eShell prompt.  Taken from here: http://www.howardism.org/Technical/Emacs/eshell-fun.html."
  (interactive)
  (insert "exit")
  (eshell-send-input)
  (delete-window))

(defun iensu/company-private-last-transformer (candidates)
  "Resort CANDIDATES putting private-ish properties last."
  (let* ((is-private #'(lambda (c) (or (string-prefix-p "_" c)
                                      (string-prefix-p "._" c))))
         (private (cl-remove-if-not '(lambda (c) (funcall is-private c)) candidates))
         (not-private (cl-remove-if '(lambda (c) (funcall is-private c)) candidates)))
    (append not-private private)))

(defun iensu/counsel-nvm-use ()
  "Forward to `nvm-use'."
  (interactive)
  (ivy-read "nvm use "
            (let (cands)
              (mapcar
               (lambda (x) (push (car x) cands))
               (reverse (nvm--installed-versions)))
              (push "default" cands))
            :require-match t
            :sort t
            :action (lambda (x)
                      (nvm-use x))
            :caller 'iensu/counsel-nvm-use))

(provide 'iensu)
;;; iensu.el ends here
