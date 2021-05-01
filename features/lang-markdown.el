(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("\\.md\\'"       . gfm-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package markdown-toc)

(defun iensu/markdown-preview ()
  "Previews the current buffer as a markdown file. Taken from https://camsaul.com/emacs-lisp/2020/06/09/emacs-lisp-intro-markdown-live-previews-part-1.html.  Relies on `pandoc'."
  (interactive)
  (let ((filename buffer-file-name))
    (message "Rendering Markdown preview of %s" filename)
    (shell-command-on-region (point-min) (point-max) "pandoc -f gfm" "*Preview Markdown Output*")
    (save-selected-window
      (switch-to-buffer-other-window "*Preview Markdown Output*")
      (let ((document (libxml-parse-html-region (point) (point-max)))
            (url (concat "file://" filename)))
        (erase-buffer)
        (shr-insert-document `(base ((href . ,url)) ,document))
        (goto-char (point-min))
        (visual-fill-column-mode)
        (setq buffer-read-only t)))))
