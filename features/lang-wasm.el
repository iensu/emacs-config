(use-package wat-mode
  :load-path (lambda () (expand-file-name "packages/wat-mode" user-emacs-directory))
  :straight nil
  :config
  (add-to-list 'org-src-lang-modes '("wat" . wat)))
