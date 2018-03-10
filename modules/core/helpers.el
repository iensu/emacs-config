;;; modules/core/helpers.el --- Helper functions -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defun iensu-add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'.  Taken from `hrs/add-auto-mode'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

(defun iensu-add-to-list (lst values)
  "Add VALUES to LST."
  (dolist (v (reverse values))
    (add-to-list lst v)))
