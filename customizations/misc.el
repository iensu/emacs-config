;(require 'iso-transl)
(define-key key-translation-map [dead-grave] (lookup-key key-translation-map "\C-x8`"))
(define-key key-translation-map [dead-acute] (lookup-key key-translation-map "\C-x8'"))
(define-key isearch-mode-map [dead-grave] nil)
(define-key isearch-mode-map [dead-acute] nil)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

(require 'git-gutter)
(global-git-gutter-mode t)
(git-gutter:linum-setup)

(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

(global-prettify-symbols-mode 1)
;; Useful for debugging
; (setq max-specpdl-size 5)
; (setq debug-on-error t)
