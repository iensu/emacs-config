;;; modules/core/editing.el --- Editing configuration

;;; Code:

;; Remove menu, toolbar and scrollbar if present
(mapc
 (lambda (mode)
   (when (fboundp mode)
     (funcall mode -1)))
 '(menu-bar-mode tool-bar-mode scroll-bar-mode))

(setq-default cursor-type '(bar . 2)

              indent-tabs-mode nil
              tab-width 2

              fill-column 80

              require-final-newline t

              sentence-end-double-space nil

              word-wrap t

              truncate-lines t

              scroll-conservatively 0
              scroll-step 4

              frame-title-format "%b (%f)"

              uniquify-buffer-name-style 'post-forward
              uniquify-separator ":")

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'prog-mode-hook
          (lambda ()
            (subword-mode 1)
            (linum-mode 1)
            (column-number-mode 1)
            (eldoc-mode 1)))

(global-prettify-symbols-mode 1)
(global-font-lock-mode 1)
(delete-selection-mode 1)
(global-hl-line-mode 1)
(global-auto-revert-mode t)

(windmove-default-keybindings)
(winner-mode 1)

(global-hl-line-mode 1)

(use-package editorconfig
  :ensure t
  :diminish editorconfig-mode
  :init
  (add-hook 'prog-mode-hook (editorconfig-mode 1))
  (add-hook 'text-mode-hook (editorconfig-mode 1)))

(use-package linum
  :ensure t
  :config
  (setq linum-format "%3d "))

(use-package linum-relative
  :ensure t
  :bind (("H-l" . linum-relative-toggle)))

(use-package iedit :ensure t)

(use-package multiple-cursors
  :ensure t
  :bind
  (("M-="         . mc/edit-lines)
   ("C-S-<right>" . mc/mark-next-like-this)
   ("C-S-<left>"  . mc/mark-previous-like-this))
  :init
  (setq mc/list-file (iensu/-config-file ".local/.mc-lists.el")))

(use-package abbrev :diminish abbrev-mode)

(use-package expand-region
  :ensure t
  :bind
  (("C-=" . er/expand-region)
   ("C-M-=" . er/contract-region)))

(use-package flycheck
  :ensure t
  :delight
  :init
  (global-flycheck-mode t))

(use-package flycheck-popup-tip
  :ensure t
  :init
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode)))

(use-package paredit
  :ensure t
  :delight)

(use-package paren
  :ensure t
  :init
  (add-hook 'prog-mode-hook (lambda () (show-paren-mode t)))
  :config
  (setq show-paren-delay 0
        show-paren-when-point-inside-paren t)
  (set-face-attribute 'show-paren-match nil
                      :background nil
                      :foreground "#fc851e"
                      :weight 'extra-bold))

(use-package rainbow-delimiters
  :ensure t
  :delight
  :config
  ;; (set-face-attribute 'rainbow-delimiters-depth-1-face nil :foreground "dark orange")
  ;; (set-face-attribute 'rainbow-delimiters-depth-2-face nil :foreground "deep pink")
  ;; (set-face-attribute 'rainbow-delimiters-depth-3-face nil :foreground "chartreuse")
  ;; (set-face-attribute 'rainbow-delimiters-depth-4-face nil :foreground "deep sky blue")
  ;; (set-face-attribute 'rainbow-delimiters-depth-5-face nil :foreground "yellow")
  ;; (set-face-attribute 'rainbow-delimiters-depth-6-face nil :foreground "orchid")
  ;; (set-face-attribute 'rainbow-delimiters-depth-7-face nil :foreground "spring green")
  ;; (set-face-attribute 'rainbow-delimiters-depth-8-face nil :foreground "sienna1")
  )

(use-package smartparens
  :ensure t
  :delight
  :init
  (smartparens-global-mode t)
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
        ("M-<left>"  . sp-backward-barf-sexp)))

(use-package subword
  :diminish subword-mode
  :init
  (defadvice subword-transpose (before subword-transpose)
    (when (looking-at "$")
      (backward-word 1))))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode))
