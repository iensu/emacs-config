;;;;;;;;
;; Auto-complete
;;;;;;;;

(require 'yasnippet)
(yas-global-mode 1)

(require 'auto-complete)
(require 'auto-complete-config)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")

(defun auto-complete-mode-maybe ()
  "No maybe for you. Only AC!"
  (unless (minibufferp (current-buffer))
    (auto-complete-mode 1)))

(setq global-auto-complete-mode t)
; (setq ac-auto-show-menu 0.1)
; (setq ac-show-menu-immediately-on-auto-complete t)

