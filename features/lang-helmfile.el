;; -*- lexical-binding: t; -*-

(load-file (expand-file-name "packages/helmfile-gotmpl-mode.el" user-emacs-directory))

(defun iensu--helmfile-gotmpl-mode-hook ()
  (display-line-numbers-mode 1)
  (column-number-mode 1)
  (flyspell-mode-off)
  (visual-line-mode -1)
  (highlight-indentation-mode 1)
  (electric-pair-mode 1))

(add-hook 'helmfile-gotmpl-mode-hook #'iensu--helmfile-gotmpl-mode-hook)
