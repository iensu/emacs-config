;;; iensu --- Some utility and other functions

(defun iensu/setup-line-numbers ()
  (linum-mode 1)
  (setq linum-format "%3d ")
  (set-face-attribute 'linum nil :height 120)
  (column-number-mode 1))

(provide 'iensu)
;;; iensu.el ends here
