;; Customizations relating to editing a buffer.

;; hippe expand
(global-set-key (kbd "M-/") 'hippie-expand)

;; adoc
(add-to-list 'auto-mode-alist (cons "\\.txt\\'" 'adoc-mode))
(add-to-list 'auto-mode-alist (cons "\\.asciidoc\\'" 'adoc-mode))

(require 'whitespace)
(add-hook 'before-save-hook 'whitespace-cleanup)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-M-=") 'er/contract-region)

;; OSX Meta remap
(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)

 ;; Lisp-friendly hippie expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Highlights matching parenthesis
(show-paren-mode 1)

;; Highlight current line
(global-hl-line-mode 1)
(set-face-background hl-line-face "#dddddd")

;; Interactive search key bindings. By default, C-s runs
;; isearch-forward, so this swaps the bindings.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; speedbar
(global-set-key (kbd "C-c C-s") 'speedbar-get-focus)

;; Don't use hard tabs
(setq-default indent-tabs-mode nil)

(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))

(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)

;; comments
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; Rainbow parens
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; tabs -> spaces
(defun die-tabs ()
  (interactive)
  (set-variable 'tab-width 2)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

;; fix weird os x kill error
(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
      (ns-get-selection-internal 'CLIPBOARD)
    (quit nil)))

;; avoid automatic indentation
(setq electric-indent-mode nil)

;; update file on external modification
(global-auto-revert-mode 1)

;; Multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Global flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
