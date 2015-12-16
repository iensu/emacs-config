(menu-bar-mode -1)

(global-linum-mode)

(when (fboundp 'tool-bar-mode)
   (tool-bar-mode -1))

;; Don't show native OS scroll bars for buffers because they're redundant
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")

(load-theme 'smyx t)
;(load-theme 'pastelmac t)
;(load-theme 'cyberpunk t)

(set-face-attribute 'default nil :height 150 :weight 'light)

(setq initial-frame-alist '((top . 50) (left . 50) (width . 160) (height . 50)))


(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; don't pop up font menu
(global-set-key (kbd "s-t") '(lambda () (interactive)))

(setq ring-bell-function 'ignore)

;; Set default font
(if (member "DejaVu Sans Mono" (font-family-list))
    (set-default-font "DejaVu Sans Mono 11"))

;; Window size modification
(global-set-key (kbd "C-S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-S-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-S-<up>") 'shrink-window)
(global-set-key (kbd "C-S-<down>") 'enlarge-window)

(column-number-mode 1)
