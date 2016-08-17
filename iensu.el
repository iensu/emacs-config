;;; iensu --- Some utility and other functions

(defun iensu/add-auto-mode (mode &rest patterns)
    "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'. Taken from `hrs/add-auto-mode'."
    (dolist (pattern patterns)
      (add-to-list 'auto-mode-alist (cons pattern mode))))

(defun iensu/prettify-javascript ()
  "Prettify some common JS reserved words."
  (utils/add-to-list 'prettify-symbols-alist
		     '("function" . ?λ)
		     '("return " . "\u2906 ")))

(defun iensu/setup-line-numbers ()
  (linum-mode 1)
  (setq linum-format "%3d ")
  (set-face-attribute 'linum nil :height 120)
  (column-number-mode 1))

(defun iensu/tern-restart-server ()
  "Restart the tern server."
  (interactive)
  (delete-process "Tern"))

(provide 'iensu)
;;; iensu.el ends here
