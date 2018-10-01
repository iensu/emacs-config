;;; init.el --- Emacs Configuration

;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- Helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun iensu--config-file (file)
  "Takes a configuration file name and returns the full file path"
  (expand-file-name file user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- Generic setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  Maximize screen real estate by disabling menu-bar, tool-bar and scroll-bar
(mapc
 (lambda (mode)
   (when (fboundp mode)
     (funcall mode -1)))
 '(menu-bar-mode tool-bar-mode scroll-bar-mode))

;; Enlarge the initial frame
(setq initial-frame-alist '((width . 120)
                            (height . 60)))

;; Improve displayed buffer names
(setq-default frame-title-format "%b (%f)"
	      uniquify-buffer-name-style 'post-forward
	      uniquify-separator ":")

;; Highlight current line
(global-hl-line-mode 1)

;; Shorter confirmation prompts
(fset 'yes-or-no-p 'y-or-n-p)

(setq default-directory "~/"
      custom-file (iensu--config-file "custom.el")

      gc-cons-threshold 100000000

      inhibit-startup-message t
      ring-bell-function 'ignore
      confirm-kill-emacs 'y-or-n-p

      create-lockfiles nil
      auto-save-default nil

      backup-directory-alist `(("." . ,(iensu--config-file ".local/.saves")))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6

      calendar-week-start-day 1

      vc-follow-symlinks t

      url-configuration-directory (iensu--config-file ".local/url")

      image-dired-dir (iensu--config-file ".local/image-dired")
      bookmark-default-file (iensu--config-file ".local/bookmarks")
      tramp-auto-save-directory (iensu--config-file ".local/tramp")

      ;; On MacOS/OSX remember to disable the built in dictionary lookup command
      ;; by running the following command followed by a restart
      ;; defaults write
      ;; com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 70 '<dict><key>enabled</key><false/></dict>'
      mac-option-modifier 'none
      mac-command-modifier 'meta
      mac-function-modifier 'hyper
      mac-right-option-modifier 'hyper

      ;; Need to setup identity using `gpg --gen-key` before using gpg
      ;; on Mac install pinentry-mac from homebrew
      ;; https://www.gnupg.org/software/pinentry/index.html
      auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc")
      epa-pinentry-mode 'loopback)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- Default editor settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default indent-tabs-mode nil
	            tab-width 2

	            fill-column 80

	            require-final-newline t

	            sentence-end-double-space nil

	            word-wrap t
	            truncate-lines t

	            scroll-conservatively 0
	            scroll-step 4
	            next-screen-context-lines 20)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

;; defaults for programming modes
(add-hook 'prog-mode-hook
          (lambda ()
            (subword-mode 1)
            (column-number-mode 1)
            (display-line-numbers-mode 1)
            (eldoc-mode 1)
            (show-paren-mode 1)))

;; Switch buffer window by pressing Shift + <direction>
(windmove-default-keybindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- Global keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-prefix-command 'iensu-map)
(global-set-key (kbd "H-1") 'iensu-map)

(dolist (keymap '(("C-<backspace>" . delete-indentation)
                  ("C-h C-s"       . iensu/toggle-scratch-buffer)
                  ("C-x C-b"       . ibuffer)
                  ("H-d"           . iensu/duplicate-line)
                  ("H-f"           . other-frame)
                  ("H-F"           . make-frame)
                  ("H-k"           . delete-frame)
                  ("H-m"           . iensu/move-file)
                  ("H-u"           . revert-buffer)
                  ("M-<backspace>" . iensu/backward-kill-word)
                  ("M-i"           . imenu)
                  ("M-o"           . occur)))
  (global-set-key (kbd (car keymap)) (cdr keymap)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- Additional packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --- Setting up package.el
(require 'package)

(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")))

(package-initialize)

;; --- Setting up use-package.el
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))

;; --- Load exec paths for access to shell executables
(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

;; --- Improving dired for directory browsing
(use-package dired+
  :load-path (lambda () (iensu--config-file "packages"))
  :config
  (when (executable-find "gls") ;; native OSX ls works differently then GNU ls
    (setq insert-directory-program "/usr/local/bin/gls"))
  (setq dired-listing-switches "-alGh --group-directories-first"
        dired-dwim-target t))

(use-package dired-subtree
  :config
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)
             (";" . dired-subtree-remove)))

;; --- Password entry dialog
(use-package pinentry :init (pinentry-start))

;; --- Remember recent files
(use-package recentf
  :init
  (recentf-mode 1)
  :init
  (run-at-time nil (* 5 60) 'recentf-save-list)
  :config
  (setq recentf-save-file (iensu--config-file ".local/recentf")
	      recentf-max-menu-items 40)
  (add-to-list 'recentf-exclude (iensu--config-file ".local/.emacs-places")))

;; --- Git
(use-package magit
  :bind (("C-x g" . magit-status)))

;; --- Project management
(use-package projectile
  :delight '(:eval (let ((project-name (projectile-project-name)))
                     (if (string-equal project-name "-")
                         ""
                       (concat " <" project-name ">"))))
  :init
  (setq projectile-cache-file (iensu--config-file ".local/projectile.cache")
        projectile-known-projects-file (iensu--config-file ".local/projectile-bookmarks.eld"))
  :config
  (projectile-global-mode)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-sort-order 'access-time)
  (let ((ignored-files '(".DS_Store" ".projectile")))
    (dolist (file ignored-files)
      (add-to-list 'projectile-globally-ignored-files file))))

;; --- Autocompletion
(use-package company
  :delight
  :init (global-company-mode)
  :config
  (setq company-idle-delay 0.3
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-auto-complete t
        company-tooltip-align-annotations t
        company-dabbrev-downcase nil
        company-auto-complete-chars nil)
  (add-hook 'emacs-lisp-mode-hook
	    (lambda ()
              (add-to-list 'company-backends 'company-elisp)))
  (eval-after-load 'company (company-quickhelp-mode 1)))

(use-package company-quickhelp
  :config
  (setq company-quickhelp-delay 1)
  (define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin))

(use-package yasnippet
  :delight yas-minor-mode
  :init
  (yas-global-mode 1)
  (setq yas-snippet-dirs (add-to-list 'yas-snippet-dirs (iensu--config-file "snippets")))
  :config
  (add-hook 'snippet-mode-hook (lambda ()
                                 (setq mode-require-final-newline nil
                                       require-final-newline nil))))

;; --- Counsel for searching and lookup (+ swiper ivy)
(use-package counsel
  :delight ivy-mode
  :init
  (ivy-mode 1)
  :bind (("C-s"     . swiper)
	       ("M-x"     . counsel-M-x)
	       ("C-x C-f"	. counsel-find-file)
         ("C-x C-r" . counsel-recentf)
	       ("<f1> f"	. counsel-describe-function)
	       ("<f1> v"	. counsel-describe-variable)
	       ("<f1> l"	. counsel-find-library)
	       ("<f2> i"	. counsel-info-lookup-symbol)
	       ("<f2> u"	. acounsel-unicode-char)
	       ("C-c k"   . counsel-ag)
	       ("C-x l"   . counsel-locate)
	       ("C-x b"   . ivy-switch-buffer)
	       ("M-y"     . counsel-yank-pop)
	       :map ivy-minibuffer-map
	       ("M-y"     . ivy-next-line))
  :config
  (setq ivy-use-virtual-buffers t
        ivy-use-selectable-prompt t
        ivy-count-format "(%d/%d) "
        ivy-magic-slash-non-match-action 'ivy-magic-non-match-create))

(use-package counsel-projectile :init (counsel-projectile-mode 1))

;; --- Flycheck for on the fly linting
(use-package flycheck
  :init
  (global-flycheck-mode t))

(use-package flycheck-popup-tip
  :init
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode)))

;; --- Flyspell for spell checking
(use-package flyspell
  :delight
  :config
  (when (executable-find "aspell")
    (setq ispell-program-name "aspell"
          ispell-extra-args '("--sug-mode=ultra")
          ispell-list-command "--list")))

(use-package flyspell-popup
  :delight
  :bind
  (:map flyspell-mode-map
        ("C-;" . flyspell-popup-correct)))

;; --- Improve editor behavior
(use-package editorconfig
  :delight
  :init
  (add-hook 'prog-mode-hook (editorconfig-mode 1))
  (add-hook 'text-mode-hook (editorconfig-mode 1)))

(use-package multiple-cursors
  :bind
  (("M-="           . mc/edit-lines)
   ("C-S-<right>"   . mc/mark-next-like-this)
   ("C-S-<left>"    . mc/mark-previous-like-this)
   ("C-S-<mouse-1>" . mc/add-cursor-on-click))
  :init
  (setq mc/list-file (iensu--config-file ".local/.mc-lists.el")))

(use-package expand-region
  :bind
  (("C-=" . er/expand-region)
   ("C-M-=" . er/contract-region)))

(use-package iedit)

(use-package smartparens
  :init
  (require 'smartparens-config)
  :bind (:map smartparens-mode-map
              ("M-s"       . sp-unwrap-sexp)
              ("C-<down>"  . sp-down-sexp)
              ("C-<up>"    . sp-up-sexp)
              ("M-<down>"  . sp-backward-down-sexp)
              ("M-<up>"    . sp-backward-up-sexp)
              ("C-<right>" . sp-forward-slurp-sexp)
              ("M-<right>" . sp-forward-barf-sexp)
              ("C-<left>"  . sp-backward-slurp-sexp)
              ("M-<left>"  . sp-backward-barf-sexp))
  :hook ((prog-mode . smartparens-mode)
         (repl-mode . smartparens-strict-mode)
         (lisp-mode . smartparens-strict-mode)
         (emacs-lisp-mode . smartparens-strict-mode)))

(use-package undo-tree
  :delight
  :init (global-undo-tree-mode))

;; --- Modeline cleanup
(use-package delight
  :config
  (delight 'global-auto-revert-mode nil t)
  (delight 'auto-revert-mode nil t))

(use-package diminish
  :init
  (add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "Eλ")))
  (add-hook 'lisp-interaction-mode (lambda () (setq mode-name "λ")))
  (add-hook 'js2-mode-hook (lambda () (setq mode-name "js2"))))

;; --- Make things pretty
(use-package dracula-theme
  :ensure t
  :init
  (load-theme 'dracula t)
  :config
  (set-face-attribute 'default nil :height 130)

  (set-face-foreground 'mode-line "black")
  (set-face-background 'mode-line "#dbffdb")

  (set-face-attribute 'show-paren-match nil
                      :background (face-background 'default)
                      :foreground "mediumspringgreen"
                      :weight 'extra-bold))

(use-package rainbow-delimiters :delight)

(use-package all-the-icons)

(use-package emojify
  :init
  (add-hook 'text-mode-hook 'emojify-mode)
  (add-hook 'org-mode-hook 'emojify-mode)
  (setq emojify-emojis-dir (iensu--config-file ".local/emojis")))

(global-prettify-symbols-mode 1)
(global-font-lock-mode 1)

(setq-default cursor-type '(bar . 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- Org mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar iensu-org-dir "~/Dropbox/org")

(defvar iensu-org-files-alist
  `((appointments     ,(concat iensu-org-dir "/appointments.org")      ?a)
    (books            ,(concat iensu-org-dir "/books.org")             ?b)
    (work-calendar    ,(concat iensu-org-dir "/calendars/work.org")    ?c)
    (private-calendar ,(concat iensu-org-dir "/calendars/private.org") ?C)
    (ekonomi          ,(concat iensu-org-dir "/ekonomi.org")           ?e)
    (journal          ,(concat iensu-org-dir "/journal.org.gpg")       ?j)
    (notes            ,(concat iensu-org-dir "/notes.org")             ?n)
    (private          ,(concat iensu-org-dir "/private.org")           ?p)
    (projects         ,(concat iensu-org-dir "/projects.org")          ?P)
    (refile           ,(concat iensu-org-dir "/refile.org")            ?r)
    (beorg-refile     ,(concat iensu-org-dir "/refile-beorg.org")      ?R)
    (richard          ,(concat iensu-org-dir "/richard.org")           ?Y)
    (work             ,(concat iensu-org-dir "/work.org")              ?w)))

(defun iensu--org-remove-file-if-match (&rest regexes)
  (let ((regex (string-join regexes "\\|")))
    (cl-remove-if (lambda (file) (string-match regex file))
                  (mapcar 'cadr iensu-org-files-alist))))

(defvar iensu-org-refile-targets
  (iensu--org-remove-file-if-match "calendars"
                                   "journal"
                                   "appointments"
                                   "refile"))

(defvar iensu-org-agenda-files
  (iensu--org-remove-file-if-match "\.org\.gpg"))

(setq org-outline-path-complete-in-steps nil)

;; Add register shortcuts for all org files
(dolist (file-props iensu-org-files-alist)
  (set-register (car (cddr file-props)) `(file . ,(cadr file-props))))

(defun iensu-org-file (key)
  "Return file path for org file matching KEY. KEY must be in `iensu-org-files-alist'."
  (cadr (assoc key iensu-org-files-alist)))

(defvar iensu-org-capture-templates-alist
  `(("t" "TODO" entry (file ,(iensu-org-file 'refile))
     ,(concat "* TODO %?\n"
              "%U\n"
              "%a\n")
     :clock-in t :clock-resume t)

    ("j" "Journal" entry (file+datetree ,(iensu-org-file 'journal))
     ,(concat "* %^{Location|Stockholm, Sweden}\n"
              "%U\n\n"
              "%?\n"))

    ("l" "Link" entry (file ,(iensu-org-file 'refile))
     ,(concat "* %? %^L %^G \n"
              "%U\n")
     :prepend t)

    ("L" "Browser Link" entry (file ,(iensu-org-file 'refile))
     ,(concat "* TO_READ %a\n"
              "%U\n")
     :prepend t :immediate-finish t)

    ("p" "Browser Link and Selection" entry (file ,(iensu-org-file 'refile))
     ,(concat "* TO_READ %^{Title}\n"
              "Source: %u, %c\n"
              "#+BEGIN_QUOTE\n"
              "%i\n"
              "#+END_QUOTE\n\n\n%?")
     :prepend t)

    ("a" "Appointment" entry (file ,(iensu-org-file 'appointments))
     ,(concat "* %^{title} %^G\n"
              "SCHEDULED: %^T\n\n"
              "%?\n"))

    ("n" "Notes" entry (file+headline ,(iensu-org-file 'notes) "Notes")
     ,(concat "* %^{Title} %^G\n"
              "%U\n\n"
              "%?\n"))
    ("b" "Book" entry (file+headline ,(iensu-org-file 'books) "Läslista")
     ,(concat "* %^{STATE|TO_READ|FINISHED} %^{} <%^{}> %^g\n\n"))))

(defun iensu--org-mode-hook ()
  (define-key org-mode-map (kbd "H-.") 'org-time-stamp-inactive)
  (dolist (lang-mode '(("javascript" . js2) ("es" . es)))
    (add-to-list 'org-src-lang-modes lang-mode))
  (auto-fill-mode 1)
  (setq org-src-fontify-natively t
        org-format-latex-options (plist-put org-format-latex-options :scale 1.5)
        truncate-lines t
        org-image-actual-width nil
        line-spacing 1
        outline-blank-line t
        org-adapt-indentation nil
        org-fontify-quote-and-verse-blocks t
        org-fontify-done-headline t
        org-fontify-whole-heading-line t
        org-hide-leading-stars t
        org-indent-indentation-per-level 2
        org-checkbox-hierarchical-statistics nil
        org-log-done 'time)
  (when (or (executable-find "ispell")
            (executable-find "aspell"))
    (flyspell-mode 1)
    (when (executable-find "aspell")
      (setq ispell-program-name "aspell"
            ispell-extra-args '("--sug-mode=ultra")))))

;; Add Swedish holidays etc to calendar
(load-file (iensu--config-file "packages/kalender.el"))

(use-package org
  :delight
  (org-mode "\u2658" :major)
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c l" . org-store-link))
  :init
  (require 'ox-md)
  (require 'ox-beamer)
  :config
  (add-hook 'org-mode-hook 'iensu--org-mode-hook)
  (org-babel-do-load-languages
   'org-babel-load-languages '((emacs-lisp . t)
                               (shell . t)
                               (js . t)
                               (python . t)))
  (org-load-modules-maybe t)
  (require 'org-protocol)
  (setq org-agenda-files iensu-org-agenda-files
        org-default-notes-file (iensu-org-file 'notes)
        org-directory iensu-org-dir
        org-capture-templates iensu-org-capture-templates-alist
        org-refile-targets '((iensu-org-refile-targets :maxlevel . 3))
        org-refile-allow-creating-parent-nodes 'confirm
        org-deadline-warning-days -7
        ;; org-agenda optimizations
        org-agenda-dim-blocked-tasks nil
        org-latex-listings t
        org-src-fontify-natively t
        org-cycle-separator-lines 1)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "TO_READ(r)" "TO_WATCH(w)" "DOING(d)" "BLOCKED(b)" "|" "CANCELLED(C)" "POSTPONED(P)" "DONE(D)"))
        org-todo-keyword-faces
        '(("BLOCKED"   . (:foreground "#dd0066" :weight bold))
          ("CANCELLED" . (:foreground "#6272a4"))
          ("POSTPONED" . (:foreground "#3388ff"))))

  ;; cleanup org-heading sizes
  (dolist (heading-num (number-sequence 1 8))
    (set-face-attribute (intern (format "org-level-%d" heading-num))
                        nil
                        :height (face-attribute 'default :height)
                        :weight 'normal)))

(use-package org-bullets
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  :config
  (setq org-bullets-bullet-list '("*")))

(use-package org-gcal
  :init
  (load-file (iensu--config-file "credentials.el"))
  (setq org-gcal-token-file (iensu--config-file ".local/org-gcal/org-gcal-token")
        org-gcal-dir (iensu--config-file ".local/org-gcal/"))
  :config
  (setq org-gcal-client-id *user-gcal-client-id*
        org-gcal-client-secret *user-gcal-client-secret*
        org-gcal-file-alist `(("jens.ostlund@futurice.com" . ,(iensu-org-file 'work-calendar)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; E-Mail
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun iensu--render-html-message ()
  (let ((dom (libxml-parse-html-region (point-min) (point-max))))
    (erase-buffer)
    (shr-insert-document dom)
    (goto-char (point-min))))

(defun iensu--mu4e-setup ()
  (global-set-key (kbd "H-2") 'mu4e)
  (define-key 'iensu-map (kbd "t") 'toggle-truncate-lines)
  (define-key 'iensu-map (kbd "c") 'mu4e-compose-new)
  (define-key 'iensu-map (kbd "m") 'mu4e-headers-search)
  (setq mail-user-agent 'mu4e-user-agent
        mu4e-mu-binary "/usr/local/bin/mu"

        mu4e-maildir "~/Mail"
        mu4e-maildir-shortcuts
        '(("/futurice/All mail" . ?F)
          ("/private/All mail" . ?P))

        mu4e-sent-messages-behavior 'delete
        mu4e-update-interval 180

        mu4e-context-policy 'pick-first
        mu4e-confirm-quit nil
        message-kill-buffer-on-exit t

        mu4e-get-mail-command "offlineimap"

        mu4e-view-show-images t
        mu4e-show-images t
        mu4e-view-image-max-width 800

        mu4e-compose-format-flowed t
        mu4e-view-show-addresses t

        mu4e-headers-fields '((:human-date . 12)
                              (:flags . 6)
                              (:tags . 16)
                              (:from . 22)
                              (:subject))

        mu4e-compose-context-policy 'ask-if-none
        mu4e-contexts
        `(,(make-mu4e-context
            :name "Futurice"
            :enter-func (lambda () (mu4e-message "Entering Futurice context"))
            :leave-func (lambda () (setq mu4e-maildir-list nil)) ; forces refresh of address list when switching context
            :match-func (lambda (msg)
                          (when msg
                            (string-match-p "^/futurice" (mu4e-message-field msg :maildir))))
            :vars '((mu4e-sent-folder   . "/futurice/sent")
                    (mu4e-drafts-folder . "/futurice/drafts")
                    (mu4e-trash-folder  . "/futurice/trash")

                    (user-mail-address  . "jens.ostlund@futurice.com")
                    (user-full-name     . "Jens Östlund")

                    (smtpmail-smtp-user . "jens.ostlund@futurice.com")))

          ,(make-mu4e-context
            :name "Private"
            :enter-func (lambda () (mu4e-message "Entering Private context"))
            :leave-func (lambda () (setq mu4e-maildir-list nil)) ; forces refresh of address list when switching context
            :match-func (lambda (msg)
                          (when msg
                            (string-match-p "^/private" (mu4e-message-field msg :maildir))))
            :vars '((mu4e-sent-folder   . "/private/sent")
                    (mu4e-drafts-folder . "/private/drafts")
                    (mu4e-trash-folder  . "/private/trash")

                    (user-mail-address  . "jostlund@gmail.com")
                    (user-full-name     . "Jens Östlund")

                    (smtpmail-smtp-user . "jostlund")))))

  (add-to-list 'mu4e-view-actions '("EWW" . iensu--mu4e-view-in-eww) t)
  (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)

  ;; message viewing settings
  (add-hook 'mu4e-view-mode-hook
            (lambda ()
              (local-set-key (kbd "<tab>") 'shr-next-link)
              (local-set-key (kbd "<backtab>") 'shr-previous-link)))
  (setq shr-color-visible-luminance-min 80))

(defun iensu--send-email-setup ()
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 465
        smtpmail-stream-type 'ssl
        smtpmail-debug-info t))

(add-hook 'message-mode-hook 'turn-on-orgtbl)
(add-hook 'message-mode-hook 'turn-on-orgstruct++)

(setq user-mail-address "jens.ostlund@futurice.com"
      user-full-name "Jens Östlund")

(let ((mu4e-path "/usr/local/share/emacs/site-lisp/mu/mu4e"))
  (when (and (executable-find "mu")
             (file-directory-p mu4e-path))
    (add-to-list 'load-path mu4e-path)
    (require 'mu4e)
    (eval-after-load "mu4e"
      (progn
        (iensu--mu4e-setup)
        (iensu--send-email-setup)))))

(use-package mu4e-alert
  :after mu4e
  :init
  (mu4e-alert-set-default-style 'notifier)
  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
  (add-hook 'after-init-hook #'mu4e-alert-enable-notifications))

(use-package org-mu4e :ensure nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Web development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --- General
(use-package emmet-mode
  :config
  (add-hook 'emmet-mode-hook
            (lambda ()
              (when (or (string-suffix-p ".jsx" (buffer-name))
                        (string-suffix-p ".tsx" (buffer-name)))
                (setq emmet-expand-jsx-className? t)))))

(defun iensu/use-prettier ()
  (and (file-exists-p (expand-file-name ".prettierrc"
                                        (locate-dominating-file (or (buffer-file-name) default-directory)
                                                                "package.json")))
       (executable-find "prettier")))

(use-package add-node-modules-path
  :load-path (lambda () (iensu--config-file "packages")))

(use-package prettier-js
  :load-path (lambda () (iensu--config-file "packages"))
  :requires add-node-modules-path
  :config
  (cl-flet ((maybe-use-prettier ()
                                (add-node-modules-path)
                                (when (iensu/use-prettier)
                                  (prettier-js-mode 1))))
    (add-hook 'web-mode-hook        #'maybe-use-prettier)
    (add-hook 'js2-mode-hook        #'maybe-use-prettier)
    (add-hook 'typescript-mode-hook #'maybe-use-prettier)))

;; --- JSON
(use-package json-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
  (setq js-indent-level 2))

;; --- CSS
(use-package rainbow-mode
  :config
  (add-hook 'css-mode-hook #'rainbow-mode))

(defun iensu--setup-css ()
  (setq css-indent-offset 2)
  (emmet-mode 1)
  (rainbow-delimiters-mode 1)
  (show-paren-mode 1))

(use-package css-mode
  :bind (:map css-mode-map
              ("C-." . company-complete-common-or-cycle))
  :config
  (add-hook 'css-mode-hook #'iensu--setup-css))

;; --- JavaScript
(defun iensu/-setup-javascript ()
  (electric-indent-mode t)
  (rainbow-delimiters-mode 1)
  (smartparens-mode 1)
  (js2-mode-hide-warnings-and-errors)
  (js2-imenu-extras-mode)
  (js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-m")
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)
  (when (executable-find "tern")
    (add-to-list 'company-backends 'company-tern)
    (tern-mode t))
  (setq js-switch-indent-offset 2
        js2-basic-offset 2
        js2-highlight-level 3)
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers '(javascript-jshint)))
  (define-key js-mode-map (kbd "M-.") nil))

(use-package js2-mode
  :mode ("\\.js\\'")
  :interpreter ("node" "nodejs")
  :config
  (add-hook 'js2-mode-hook #'iensu/-setup-javascript))

(use-package rjsx-mode
  :mode ("\\.jsx\\'")
  :config
  (add-hook 'rjsx-mode-hook (lambda ()
                              (emmet-mode)
                              (setq emmet-expand-jsx-className? t)))
  (add-hook 'rjsx-mode-hook 'iensu/-setup-javascript)
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode))

(use-package js2-refactor
  :delight js2-refactor-mode)

(use-package xref-js2
  :defer nil)

(use-package company-tern)

(use-package mocha)

(use-package nvm)

(use-package tern
  :delight " 鰺刺"
  :init
  (add-to-list 'auto-mode-alist '("\\.tern-project$" . json-mode))
  :config
  (define-key tern-mode-keymap (kbd "M-.") nil)
  (define-key tern-mode-keymap (kbd "M-,") nil))

(use-package web-mode
  :init
  (dolist (ext (list "\\.html$" "\\.hbs$" "\\.handlebars$" "\\.jsp$" "\\.eex$"))
    (add-to-list 'auto-mode-alist `(,ext . web-mode)))
  :config
  (setq web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-markup-indent-offset 2
        web-mode-attr-indent-offset 2
        web-mode-attr-value-indent-offset 2
        web-mode-enable-css-colorization t
        web-mode-enable-current-element-highlight t
        web-mode-enable-current-column-highlight t)
  (add-hook 'web-mode-hook (lambda () (yas-activate-extra-mode 'js-mode)))
  (setq-default flychqeck-disabled-checkers
                (append flycheck-disabled-checkers '(javascript-jshint)))
  (add-hook 'web-mode-hook 'emmet-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode))

;; --- TypeScript
(defun iensu/setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled)
        typescript-indent-level 2)
  (eldoc-mode +1)
  (company-mode +1))

(use-package typescript-mode
  :delight
  (typescript-mode "TS" :major)
  :config
  (add-to-list 'auto-mode-alist '("\\.ts$" . typescript-mode))
  (add-hook 'typescript-mode-hook (lambda () (smartparens-strict-mode 1))))

(use-package tide
  :delight " 潮"
  :bind (:map tide-mode-map
              ("C-."     . company-files)
              ("M-."     . tide-jump-to-definition)
              ("M-,"     . tide-jump-back)
              ("C-c l d" . tide-documentation-at-point)
              ("C-c l l" . tide-references)
              ("C-c l e" . tide-project-errors)
              ("C-c l f" . tide-fix)
              ("C-c l n" . tide-rename-symbol)
              ("C-c l r" . tide-refactor)
              ("C-c t"   . npm-test-run-tests))
  :init
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  :after (typescript-mode company flycheck web-mode)
  :hook ((typescript-mode . iensu/setup-tide-mode)
         (typescript-mode . tide-hl-identifier-mode)
         (web-mode . (lambda ()
                       (when (and buffer-file-name
                                  (string-equal "tsx" (file-name-extension buffer-file-name)))
                         (iensu/setup-tide-mode)))))
  :config (if (not (iensu/use-prettier))
              (add-hook 'before-save-hook 'tide-format-before-save)))

;; --- Endpoint testing
(use-package restclient
  :init
  (add-to-list 'auto-mode-alist '("\\.rest$" . restclient-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- Other packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("\\.md\\'"       . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (when (executable-find "macdown")
    (setq  markdown-open-command "macdown"))
  (when (or (executable-find "ispell")
            (executable-find "aspell"))
    (flyspell-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- Additional custom commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun iensu/backward-kill-word ()
  "Kill word backward and trim whitespace until previous word."
  (interactive)
  (flet ((multiple-preceding-blanks ()
           (string-match-p "\\s-\\s-" (char-to-string (char-before)))))
    (if (multiple-preceding-blanks)
        (delete-char -1)
      (paredit-backward-kill-word))
    (while (multiple-preceding-blanks)
      (delete-char -1))))

(defun iensu/duplicate-line (n)
  "Copy the current line N times and insert it below."
  (interactive "P")
  (let ((cur-pos (point)))
    (dotimes (i (prefix-numeric-value n))
      (move-beginning-of-line nil)
      (kill-line)
      (yank)
      (newline)
      (insert (string-trim-right (car kill-ring)))
      (goto-char cur-pos))))

(defun iensu/toggle-scratch-buffer ()
  "Based on a great idea from Eric Skoglund (https://github.com/EricIO/emacs-configuration/)."
  (interactive)
  (if (string-equal (buffer-name (current-buffer))
                    "*scratch*")
      (switch-to-buffer (other-buffer))
    (switch-to-buffer "*scratch*")))

(defun iensu/move-file (new-location)
  "Write this file to NEW-LOCATION, and delete the old one.  Copied from http://zck.me/emacs-move-file."
  (interactive (list (if buffer-file-name
                         (read-file-name "Move file to: ")
                       (read-file-name "Move file to: "
                                       default-directory
                                       (expand-file-name (file-name-nondirectory (buffer-name))
                                                         default-directory)))))
  (when (file-exists-p new-location)
    (delete-file new-location))
  (let ((old-location (buffer-file-name)))
    (write-file new-location t)
    (when (and old-location
               (file-exists-p new-location)
               (not (string-equal old-location new-location)))
      (delete-file old-location))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- Loading final configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load custom-file 'noerror)

(let ((private-settings (expand-file-name "private.el" user-emacs-directory)))
  (when (file-exists-p private-settings)
    (load private-settings)))

(provide 'init)
;;; init.el ends here
