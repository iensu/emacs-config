;;;  -*- lexical-binding: t -*-

;;;;;;;; VARIABLES

(defvar iensu-modeline--color-ok "green")
(defvar iensu-modeline--color-danger "red")
(defvar iensu-modeline--color-warning "orange")
(defvar iensu-modeline--color-shaded "gray85")
(defvar iensu-modeline--default-text-height 0.9)

;;;;;;;; LAYOUT HELPERS

(defun --iensu-modeline/space-between (left right)
  (let* ((pad-string (lambda (str padding)
                       (format (concat "%" (int-to-string padding) "s") str)))

         (padded-right (format (format "%%%ds" (- (window-total-width) (length left)))
                               right)))
    (concat left padded-right)))

;;;;;;;; RENDER HELPERS

(defun --iensu-modeline/buffer-file-state ()
  (pcase (format-mode-line "%*")
    ("*" (propertize (all-the-icons-faicon "asterisk" :height 1.0 :v-adjust -0.0)
                     'face `(:family ,(all-the-icons-faicon-family) :foreground ,iensu-modeline--color-warning)))
    ("-" (propertize (all-the-icons-material "save" :height 1.2 :v-adjust -0.1)
                     'face `(:family ,(all-the-icons-material-family) :height 1.3 :foreground ,iensu-modeline--color-ok)))
    ("%" (propertize (all-the-icons-octicon "lock" :height 1.2 :v-adjust 0.1)
                     'face `(:family ,(all-the-icons-octicon-family) :foreground ,iensu-modeline--color-shaded)))
    (_ " ")))

(defun --iensu-modeline/buffer-name ()
  (cond
   ((buffer-file-name) (file-name-nondirectory (buffer-file-name)))
   (t (buffer-name))))

(defun --iensu-modeline/buffer-position ()
  (concat (propertize
           (let ((pos (format-mode-line "%p")))
             (pcase pos
               ("Top"    "0%%")
               ("Bottom" "100%%")
               ("All"    "100%%")
               (_        (format "%s%%" pos))))
           'face `(:height ,iensu-modeline--default-text-height))
          "%%%"))

(defun --iensu-modeline/coding-system ()
  (setq eol-mnemonic-unix      ":LF"
        eol-mnemonic-dos       ":CRLF"
        eol-mnemonic-mac       ":CR"
        eol-mnemonic-undecided ":?")
  (propertize (format-mode-line " [%Z]") 'face `(:height 0.7)))

(defun --iensu-modeline/cursor-position ()
  (propertize "(%l,%c) " 'face `(:height ,iensu-modeline--default-text-height)))

(defun --iensu-modeline/flycheck-status ()
  (let* ((text (pcase flycheck-last-status-change
                 (`finished (if flycheck-current-errors
                                (let ((errors (let-alist (flycheck-count-errors flycheck-current-errors)
                                                (or .error 0)))
                                      (warnings (let-alist (flycheck-count-errors flycheck-current-errors)
                                                  (or .warning 0))))
                                  (concat (propertize (format "✖ %s " errors)
                                                      'face `(:height ,iensu-modeline--default-text-height :foreground ,iensu-modeline--color-danger))
                                          (propertize (format "⚠ %s" warnings)
                                                      'face `(:height ,iensu-modeline--default-text-height :foreground ,iensu-modeline--color-warning))))
                              (propertize "✔ No Issues"
                                          'face `(:height ,iensu-modeline--default-text-height :foreground ,iensu-modeline--color-ok))))
                 (`running     (propertize "⟲ Running"
                                           'face `(:height ,iensu-modeline--default-text-height :foreground ,iensu-modeline--color-ok)))
                 (`no-checker  (propertize "⚠ No Checker"
                                           'face `(:height ,iensu-modeline--default-text-height :foreground ,iensu-modeline--color-warning)))
                 (`not-checked (propertize "✖ Disabled"
                                           'face `(:height ,iensu-modeline--default-text-height :foreground ,iensu-modeline--color-shaded)))
                 (`errored     (propertize "⚠ Error"
                                           'face `(:height ,iensu-modeline--default-text-height :foreground ,iensu-modeline--color-danger)))
                 (`interrupted "")
                 (`suspicious  ""))))
    (propertize text
                'help-echo "Show Flycheck Errors"
                'mouse-face '(:box 1)
                'local-map (make-mode-line-mouse-map
                            'mouse-1 (lambda () (interactive) (flycheck-list-errors))))))

(defun --iensu-modeline/lsp-server ()
  (when (and lsp-mode lsp--buffer-workspaces)
    (propertize (apply 'concat (--map (format " LSP:%s" (lsp--workspace-print it))
                                      lsp--buffer-workspaces))
                'face `(:height 0.7))))

(defun --iensu-modeline/major-mode ()
  (propertize (format "   %s"
                      (all-the-icons-icon-for-mode (buffer-local-value 'major-mode (current-buffer))
                                                   :height 1.0 :v-adjust 0.0))))

(defun --iensu-modeline/project-name ()
  (let ((project-name (projectile-project-name)))
    (when (and project-name (not (string-equal project-name "-")))
      (concat
       (propertize (all-the-icons-octicon "graph" :height 1.0 :v-adjust 0.1)
                   'face `(:family ,(all-the-icons-octicon-family) :foreground ,iensu-modeline--color-shaded))
       (propertize (format " %s " project-name) 'face `(:height ,iensu-modeline--default-text-height :foreground ,iensu-modeline--color-shaded))))))

(defun --iensu-modeline/spell-checking ()
  (when flyspell-mode
    (concat
     (propertize (all-the-icons-octicon "book" :height 1.2 :v-adjust 0.0)
                 'face `(:family ,(all-the-icons-octicon-family)))
     (propertize (format " %s" (or ispell-local-dictionary ispell-dictionary))))))

(defun --iensu-modeline/version-control ()
  (when vc-mode
    (cond
     ((string-match "Git[:-]" vc-mode)
      (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
        (concat
         (propertize (format "%s" (all-the-icons-octicon "git-branch"  :v-adjust 0.0))
                     'face `(:height 1.0 :family ,(all-the-icons-octicon-family) :foreground ,iensu-modeline--color-shaded)
                     'display '(raise 0.1))
         (propertize (format " %s" branch) 'face `(:height ,iensu-modeline--default-text-height :foreground ,iensu-modeline--color-shaded)))))

     (t (format "%s" vc-mode)))))

;;;;;;;; MAIN FUNCTIONS

(defun iensu-modeline/enable ()
  (interactive)
  (custom-set-faces
   '(mode-line ((t (:background "#44475a" :foreground "white" :box (:line-width 4 :color "#44475a"))))))
  (setq-default mode-line-format
                '((:eval (--iensu-modeline/space-between
                          (format-mode-line
                           '(" "
                             (   :eval (--iensu-modeline/buffer-file-state))
                             " "
                             (   :eval (--iensu-modeline/buffer-name))
                             (   :eval (--iensu-modeline/coding-system))
                             (   :eval (--iensu-modeline/lsp-server))
                             " "
                             (9   :eval (--iensu-modeline/cursor-position))
                             (10 :eval (--iensu-modeline/buffer-position))
                             " "
                             (   :eval (--iensu-modeline/flycheck-status))
                             "  "
                             (   :eval (--iensu-modeline/spell-checking))))
                          (format-mode-line
                           '((   :eval (--iensu-modeline/project-name))
                             (   :eval (--iensu-modeline/version-control))
                             (   :eval (--iensu-modeline/major-mode)))))))))

(provide 'iensu-modeline/enable)
