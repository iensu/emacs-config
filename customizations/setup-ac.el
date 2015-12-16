;;;;;;;;
;; Auto-complete
;;;;;;;;

(require 'yasnippet)
(yas-global-mode 1)

;; auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
(add-hook 'after-init-hook 'global-auto-complete-mode)
(add-hook 'after-init-hook 'global-company-mode)

;; company-mode
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

; (company-quickhelp-mode 1)
; (setq ac-auto-show-menu 0.1)
; (setq ac-show-menu-immediately-on-auto-complete t)
