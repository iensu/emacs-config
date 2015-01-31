;;;;;;;;
;; Auto-complete
;;;;;;;;

(require 'auto-complete)
(require 'auto-complete-config)

(defun auto-complete-mode-maybe ()
  "No maybe for you. Only AC!"
  (unless (minibufferp (current-buffer))
    (auto-complete-mode 1)))

(setq global-auto-complete-mode t)
; (setq ac-auto-show-menu 0.1)
; (setq ac-show-menu-immediately-on-auto-complete t)

