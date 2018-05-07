;;; modules/core/ui.el --- UI -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; Remove menu, toolbar and scrollbar if present
(mapc
 (lambda (mode)
   (when (fboundp mode)
     (funcall mode -1)))
 '(menu-bar-mode tool-bar-mode scroll-bar-mode))

(setq-default cursor-type '(bar . 2)

              frame-title-format "%b (%f)"
              uniquify-buffer-name-style 'post-forward
              uniquify-separator ":")

(setq initial-frame-alist '((width . 120)
                            (height . 60)))

(global-hl-line-mode 1)
(global-prettify-symbols-mode 1)
(global-font-lock-mode 1)

(add-hook 'prog-mode-hook
          (lambda ()
            (linum-mode 1)
            (column-number-mode 1)
            (eldoc-mode 1)))

(use-package dracula-theme
  :ensure t
  :init
  (load-theme 'dracula t)
  :config
  (set-face-attribute 'show-paren-match nil
                      :background (face-background 'default)
                      :foreground "mediumspringgreen"
                      :weight 'extra-bold))

(use-package rainbow-delimiters
  :delight)

(use-package linum
  :config
  (setq linum-format "%3d "))

(use-package linum-relative
  :bind (("H-l" . linum-relative-toggle)))

(use-package all-the-icons)

(use-package emojify
  :init
  (add-hook 'text-mode-hook 'emojify-mode)
  (add-hook 'org-mode-hook 'emojify-mode)
  (setq emojify-emojis-dir (iensu--config-file ".local/emojis")))

(use-package diminish
  :init
  (add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "Eλ")))
  (add-hook 'lisp-interaction-mode (lambda () (setq mode-name "λ")))
  (add-hook 'js2-mode-hook (lambda () (setq mode-name "js2"))))

(use-package delight
  :config
  (delight 'global-auto-revert-mode nil t)
  (delight 'auto-revert-mode nil t))

(use-package eldoc :delight)

(use-package time
  :init
  (display-time-mode t)
  (setq display-time-24hr-format t)
  :config
  (setq display-time-day-and-date nil))
