(defun iensu--custom-modeline-modified ()
  (let ((buffer-state (format-mode-line "%*")))
    (cond
     ((string-equal buffer-state "*")
      (propertize (all-the-icons-faicon "asterisk" :height 1.0 :v-adjust -0.0)
                  'face `(:family ,(all-the-icons-faicon-family) :foreground "orange")))
     ((string-equal buffer-state "-" )
      (propertize (all-the-icons-material "save" :height 1.2 :v-adjust -0.1)
                  'face `(:family ,(all-the-icons-material-family) :height 1.3 :foreground "green")))
     ((string-equal buffer-state "%")
      (propertize (all-the-icons-octicon "lock" :height 1.2 :v-adjust 0.1)
                  'face `(:family ,(all-the-icons-octicon-family) :foreground "gray85")))
     (t " "))))

(defun iensu--custom-modeline-space-between (left right)
  (let* ((pad-string (lambda (str padding)
                       (format (concat "%" (int-to-string padding) "s") str)))

         (padded-right (format (format "%%%ds" (- (window-total-width) (length left)))
                               right)))
    (concat left padded-right)))

(defun iensu--custom-modeline-icon-vc ()
  (when vc-mode
    (cond
     ((string-match "Git[:-]" vc-mode) (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
                                         (concat
                                          (propertize (format "%s" (all-the-icons-octicon "git-branch"  :v-adjust 0.0))
                                                      'face `(:height 1.0 :family ,(all-the-icons-octicon-family) :foreground "gray85")
                                                      'display '(raise 0.1))
                                          (propertize (format " %s" branch) 'face '(:height 0.9 :foreground "gray85")))))
     (t (format "%s" vc-mode)))))

(defun iensu--custom-modeline-flycheck-status ()
  (let* ((text (pcase flycheck-last-status-change
                 (`finished (if flycheck-current-errors
                                (let ((errors (let-alist (flycheck-count-errors flycheck-current-errors)
                                                (or .error 0)))
                                      (warnings (let-alist (flycheck-count-errors flycheck-current-errors)
                                                  (or .warning 0))))
                                  (concat (propertize (format "✖ %s " errors) 'face '(:height 0.9 :foreground "red"))
                                          (propertize (format "⚠ %s" warnings) 'face '(:height 0.9 :foreground "orange"))))
                              (propertize "✔ No Issues" 'face '(:height 0.9 :foreground "green"))))
                 (`running     (propertize "⟲ Running" 'face '(:height 0.9 :foreground "green")))
                 (`no-checker  (propertize "⚠ No Checker" 'face '(:height 0.9 :foreground "orange")))
                 (`not-checked (propertize "✖ Disabled" 'face '(:height 0.9 :foreground "gray85")))
                 (`errored     (propertize "⚠ Error" 'face '(:height 0.9 :foreground "red")))
                 (`interrupted "")
                 (`suspicious  ""))))
    (propertize text
                'help-echo "Show Flycheck Errors"
                'mouse-face '(:box 1)
                'local-map (make-mode-line-mouse-map
                            'mouse-1 (lambda () (interactive) (flycheck-list-errors))))))

(defun iensu--custom-modeline-spell-checking ()
  (when flyspell-mode
    (concat
     (propertize (all-the-icons-octicon "book" :height 1.2 :v-adjust 0.0)
                 'face `(:family ,(all-the-icons-octicon-family)))
     (propertize (format " %s" (or ispell-local-dictionary ispell-dictionary))))))

(defun iensu--custom-modeline-buffer-position ()
  (propertize (let ((pos (format-mode-line "%p")))
                (pcase pos
                  ("Top"    "0%%")
                  ("Bottom" "100%%")
                  ("All"    "100%%")
                  (_        (format "%s%%" pos))))
              'face '(:height 0.9)))

(defun iensu--custom-modeline-buffer-name ()
  (cond
   ((buffer-file-name) (file-name-nondirectory (buffer-file-name)))
   (t (buffer-name))))

(defun iensu--custom-modeline-buffer-mode ()
  (propertize (all-the-icons-icon-for-mode
               (buffer-local-value 'major-mode (current-buffer)) :height 1.0 :v-adjust 0.0)))

(defun iensu--custom-modeline-project-name ()
  (let ((project-name (projectile-project-name)))
    (when (and project-name (not (string-equal project-name "-")))
      (concat
       (propertize (all-the-icons-octicon "graph" :height 1.0 :v-adjust 0.1)
                   'face `(:family ,(all-the-icons-octicon-family) :foreground "gray85"))
       (propertize (format " %s " project-name) 'face '(:height 0.9 :foreground "gray85"))))))

(defun iensu/custom-modeline ()
  (interactive)
  (custom-set-faces
   '(mode-line ((t (:background "#44475a" :foreground "white" :box (:line-width 4 :color "#44475a"))))))
  (setq mode-line-format
                '((:eval (let ((left-side (format-mode-line
                                           '(" "
                                             (   :eval (iensu--custom-modeline-modified))
                                             " "
                                             (   :eval (iensu--custom-modeline-buffer-name))
                                             "  "
                                             (   :eval (iensu--custom-modeline-project-name))
                                             (   :eval (iensu--custom-modeline-icon-vc)))))
                               (right-side (format-mode-line
                                            '(" "
                                              (    :eval (iensu--custom-modeline-spell-checking))
                                              " "
                                              (-14 :eval (iensu--custom-modeline-flycheck-status))
                                              " "
                                              (11  :eval (propertize "(%l,%c) " 'face '(:height 0.9)))
                                              (6   :eval (concat (iensu--custom-modeline-buffer-position) "%%"))
                                              " "
                                              (    :eval (iensu--custom-modeline-buffer-mode))))))
                           (iensu--custom-modeline-space-between left-side right-side))))))

(provide 'iensu/custom-modeline)
