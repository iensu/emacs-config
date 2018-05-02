;;; modules/core/editing.el --- Editing configuration

;;; Code:

(setq-default indent-tabs-mode nil tab-width 2
              fill-column 80

              require-final-newline t

              sentence-end-double-space nil

              word-wrap t

              truncate-lines t

              scroll-conservatively 0
              scroll-step 4)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'prog-mode-hook
          (lambda ()
            (subword-mode 1)))

(delete-selection-mode 1)

(global-auto-revert-mode t)
(setq  global-auto-revert-non-file-buffers t
       auto-revert-verbose nil)

(windmove-default-keybindings)
(winner-mode 1)

(use-package editorconfig
  :delight
  :init
  (add-hook 'prog-mode-hook (editorconfig-mode 1))
  (add-hook 'text-mode-hook (editorconfig-mode 1)))

(use-package iedit)

(use-package multiple-cursors
  :bind
  (("M-="         . mc/edit-lines)
   ("C-S-<right>" . mc/mark-next-like-this)
   ("C-S-<left>"  . mc/mark-previous-like-this))
  :init
  (setq mc/list-file (iensu--config-file ".local/.mc-lists.el")))

(use-package abbrev :delight)

(use-package expand-region
  :bind
  (("C-=" . er/expand-region)
   ("C-M-=" . er/contract-region)))

(use-package paredit :delight)

(use-package paren
  :init
  (add-hook 'prog-mode-hook (lambda () (show-paren-mode t)))
  :config
  (setq show-paren-delay 0
        show-paren-when-point-inside-paren t
        show-paren-style 'parenthesis))

(use-package emacs
  :delight
  (auto-fill-function " \u2630"))

(use-package smartparens
  :init
  (require 'smartparens-config)
  :bind
  (:map smartparens-mode-map
        ("M-s" . sp-unwrap-sexp)
        ("C-<down>" . sp-down-sexp)
        ("C-<up>"   . sp-up-sexp)
        ("M-<down>" . sp-backward-down-sexp)
        ("M-<up>"   . sp-backward-up-sexp)
        ("C-<right>" . sp-forward-slurp-sexp)
        ("M-<right>" . sp-forward-barf-sexp)
        ("C-<left>"  . sp-backward-slurp-sexp)
        ("M-<left>"  . sp-backward-barf-sexp))
  :config
  (add-hook 'prog-mode-hook
            (lambda () (smartparens-mode t))))

(use-package subword
  :delight
  :init
  (defadvice subword-transpose (before subword-transpose)
    (when (looking-at "$")
      (backward-word 1))))

(use-package undo-tree
  :delight
  :init
  (global-undo-tree-mode))
