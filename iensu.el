;;; iensu --- Some utility and other functions

(defun iensu/add-auto-mode (mode &rest patterns)
    "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'. Taken from `hrs/add-auto-mode'."
    (dolist (pattern patterns)
      (add-to-list 'auto-mode-alist (cons pattern mode))))

(defun iensu/duplicate-line ()
  "Copy the current line and insert it below"
  (interactive)
  (let ((cur-pos (point)))
    (move-beginning-of-line 1)
    (kill-line)
    (yank)
    (newline)
    (yank)
    (goto-char cur-pos)))

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
    (let ((eslint-path (concat (projectile-project-root) "node_modules/.bin/eslint")))
      (when (file-exists-p eslint-path)
        (setq flycheck-javascript-eslint-executable eslint-path))))

(defun iensu/toggle-scratch-buffer ()
  "Based on a great idea from Eric Skoglund (https://github.com/EricIO/emacs-configuration/)."
  (interactive)
  (if (string-equal (buffer-name (current-buffer))
                    "*scratch*")
      (switch-to-buffer (other-buffer))
    (switch-to-buffer "*scratch*")))

(provide 'iensu)
;;; iensu.el ends here
