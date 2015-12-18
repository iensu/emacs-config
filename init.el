;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE INIT
;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar packages
  '(magit
    git-gutter

    paredit
    ))

(if (eq system-type 'darwin)
    (add-to-list 'packages 'exec-path-from-shell))

(dolist (p packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;;;;;;;;;;;;;;;;;;;;
;; BASIC SETTINGS
;;;;;;;;;;;;;;;;;;;;;
(fset 'yes-or-no-p 'y-or-n-p)

;; OSX Meta remap
(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)

;;;;;;;;;;;;;;;;;;;;;
;; EDITING
;;;;;;;;;;;;;;;;;;;;;
(define-key key-translation-map [dead-grave] (lookup-key key-translation-map "\C-x8`"))
(define-key key-translation-map [dead-acute] (lookup-key key-translation-map "\C-x8'"))
(define-key isearch-mode-map [dead-grave] nil)
(define-key isearch-mode-map [dead-acute] nil)

(setq create-lockfiles nil)

(global-auto-revert-mode 1)

;;;;;;;;;;;;;;;;;;;;;
;; UI
;;;;;;;;;;;;;;;;;;;;;
(show-paren-mode 1)

(global-hl-line-mode 1)
;(set-face-background hl-line-face "#111111")

;;;;;;;;;;;;;;;;;;;;;
;; GIT
;;;;;;;;;;;;;;;;;;;;;
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

(require 'git-gutter)
(global-git-gutter-mode t)
(git-gutter:linum-setup)

;;;;;;;;;;;;;;;;;;;;;
;; LANGUAGE SETTINGS
;;;;;;;;;;;;;;;;;;;;;

;; elisp
(add-hook 'emacs-lisp-mode-hook (lambda ()
				  (enable-paredit-mode)
				  (turn-on-eldoc-mode)))
