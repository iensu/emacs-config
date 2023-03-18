;;; Code:

;;;; Package installation and management

;; Do not issue warnings during byte compilation. Not sure if this is a good idea though :D
(setq byte-compile-warnings nil)
(setq native-comp-async-report-warnings-errors nil)

;; This section initializes `straight' as the package manager and installs `use-package' which is
;; used to install packages.

(setq package-enable-at-startup nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)

;; Make system path variables accessible in Emacs
(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-check-startup-files nil)
  :init
  (exec-path-from-shell-initialize))

(when (and (fboundp 'native-comp-available-p)
      	   (native-comp-available-p))
	(message "Native compilation enabled!")
  (setq comp-deferred-compilation t))


;;;; System local configuration

;; This section handles configurations that are local to the system and thus should not be under
;; version control, for instance credentials and system specific feature flags.

;; System local variables which can be set in local-settings.el
(defvar iensu-org-dir nil
  "Directory containing Org files.")

(defvar iensu-org-refile-targets nil
  "Org files which can be used as refiling targets.")

(defvar iensu-org-agenda-files nil
  "Org files which should be used by org-agenda to generate TODO lists etc.")

(defvar iensu-org-capture-templates nil
  "Capture templates to be used by Org mode.")

(defvar iensu-email-directory "~/Mail"
  "Path to email directory.")

(defvar iensu-enabled-features-alist nil
  "Locally enabled features. Available features are stored in the `features/' directory.")


;; Load settings
(let ((local-settings-file (expand-file-name "local-settings.el" user-emacs-directory)))
  (when (file-exists-p local-settings-file)
    (load-file (expand-file-name "local-settings.el"
				                         user-emacs-directory))))


;;;; Helper functions

(defun iensu-add-to-list (list &rest items)
  "Add multiple items to a list."
  (dolist (item items)
    (add-to-list list item)))


;;;; Basic setup

;; This contains some basic default configuration which does not depend on any external packages.

;; Cleanup the UI by removing the menu tool and scroll bars.
(mapc (lambda (mode)
        (when (fboundp mode)
          (funcall mode -1)))
      '(menu-bar-mode tool-bar-mode scroll-bar-mode))

;; Remove the warning bell.
(setq ring-bell-function 'ignore)

;; Remove the GNU/Emacs startup screen to boot direct to the Scratch buffer.
(setq inhibit-startup-screen t)

;; The week starts on Monday.
(setq calendar-week-start-day 1)

(setq calendar-intermonth-text
      '(propertize
        (format "%2d"
                (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'font-lock-warning-face))

(setq calendar-intermonth-header
      (propertize "Wk" 'font-lock-face 'font-lock-keyword-face))


;; Make sure that buffer names become unique when opening multiple files of the same name.
(setq-default frame-title-format "%b (%f)"
              uniquify-buffer-name-style 'post-forward
              uniquify-separator ":")

;; Ask before killing Emacs.
(setq confirm-kill-emacs 'y-or-n-p)

;; Always just ask y/n instead of yes/no
(fset #'yes-or-no-p #'y-or-n-p)

;; Put any Emacs generated customizations into ./custom.el instead of ./init.el.
(setq custom-file (expand-file-name "custom.el"
                                    user-emacs-directory))

;; Prefer to recompile newer files instead of using already compiled code
(setq load-prefer-newer t)

;; Improve performance by increasing the garbage collector threshold and max LISP evaluation depth.
(setq gc-cons-threshold 100000000
      max-lisp-eval-depth 2000
      read-process-output-max (* 1024 1024))

;; Move backups and auto-saves
(setq create-lockfiles nil
      backup-directory-alist `(("." . ,(expand-file-name ".local/backups/" user-emacs-directory)))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      frame-inhibit-implied-resize 1

      delete-by-moving-to-trash t
      undo-limit 8000000

      auto-save-list-file-name (expand-file-name ".local/auto-saves-list" user-emacs-directory))

;; Don't allow eldoc to display more than one line in the echo area
(setq eldoc-echo-area-use-multiline-p nil)

(pixel-scroll-mode 1)

;; Enable autosaves
(auto-save-mode 1)

;; Remember recent files
(use-package recentf
  :custom
  (recentf-max-menu-items 50)
  :config
  (recentf-load-list)
  :init
  (recentf-mode 1)
  (setq recentf-save-file (expand-file-name ".local/recentf" user-emacs-directory)))

;; Keep some files in ~/.emacs.d/.local to avoid cluttering the configuration root directory.
(setq url-configuration-directory (expand-file-name ".local/uri/" user-emacs-directory)
      image-dired-dir (expand-file-name ".local/image-dired-thumbnails/" user-emacs-directory)
      bookmark-default-file (expand-file-name ".local/bookmarks" user-emacs-directory)
      tramp-auto-save-directory (expand-file-name ".local/tramp-autosaves/" user-emacs-directory))

;; Setup authentication file.
(setq auth-sources '("~/.authinfo.gpg"
                     "~/.netrc"))

;; Auto scroll through output in compilation buffers.
(setq compilation-scroll-output t)

;; Save bookmarks when changed
(setq bookmark-save-flag 1)


;;;; Editor default settings

;; This section changes the default text editing behavior of Emacs so it is more inline with my
;; expectations.

;; Use spaces instead of tabs.
(setq-default indent-tabs-mode nil
              tab-width 2)

;; Read-only buffers are visited in `view-mode'.
(setq view-read-only t)

(setq-default require-final-newline t) ; Files should always have a final newline.

(setq-default sentence-end-double-space nil) ; Sentence end does not require two spaces.

;; Fix buffer scrolling behavior.
(setq-default scroll-conservatively 0
              scroll-step 4
              next-screen-context-lines 20)

(add-hook 'before-save-hook 'delete-trailing-whitespace) ; Delete trailing whitespace on save.

(delete-selection-mode 1) ; Replace selected text with typed text.

;; Enable narrowing to region and page. Pages are delimited by ^L.
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Use the editorconfig package to conform to project formatting rules if present.
(use-package editorconfig
  :hook
  (prog-mode . editorconfig-mode)
  (text-mode . editorconfig-mode))

;; Make parentheses pretty
(use-package rainbow-delimiters
  :hook
  (scheme-mode . rainbow-delimiters-mode)
  (emacs-lisp-mode . rainbow-delimiters-mode)
  (lisp-mode . rainbow-delimiters-mode)
  (lisp-interaction-mode . rainbow-delimiters-mode)
  (geiser-repl-mode . rainbow-delimiters-mode))

;; Enable multiple cursors for convenient editing. Use `iedit' for quick and dirty multi-cursor
;; functionality.
(use-package iedit)
(use-package multiple-cursors
  :bind
  (("M-="           . mc/edit-lines)
   ("C-S-<right>"   . mc/mark-next-like-this)
   ("C-S-<left>"    . mc/mark-previous-like-this)
   ("C-S-<mouse-1>" . mc/add-cursor-on-click))
  :custom
  (mc/list-file (expand-file-name ".local/.mc-lists.el" user-emacs-directory)))

;; Expand region from current region or point.
(use-package expand-region
  :bind
  (("C-=" . er/expand-region)
   ("C-M-=" . er/contract-region)))

;;;;; Programming mode related settings

(defun iensu--prog-mode-hook ()
  "Defaults for programming modes"
  (subword-mode 1) ; delimit words at camelCase boundries
  (eldoc-mode 1) ; display documentation in minibuffer
  (display-line-numbers-mode 1) ;; display line numbers
  (show-paren-mode 1) ; highlight matching parentheses
  (setq-default show-paren-when-point-in-periphery t
                show-paren-when-point-inside-paren t)
  (hs-minor-mode 1) ; hide-show code and comment blocks
  (outline-minor-mode 1)) ; Navigate by outlines

(add-hook 'prog-mode-hook #'iensu--prog-mode-hook)

;; Manipulate parentheses and other code structures.
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
  :hook
  (prog-mode . smartparens-mode) ; non-strict by default, but keep strict in LISPs
  (repl-mode . smartparens-strict-mode)
  (lisp-mode . smartparens-strict-mode)
  (ielm-mode . smartparens-strict-mode)
  (emacs-lisp-mode . smartparens-strict-mode))

;; Prettify compilation-mode buffers
(use-package xterm-color
  :init
  (defun iensu--advice-compilation-filter (f proc string)
    ;; Apply `xterm-color' only to real compilation buffers, and not buffers which rely on the
    ;; color codes for parsing (ag.el, rg.el)
    ;; More info: https://github.com/atomontage/xterm-color/issues/37
    (funcall f proc (if (string-prefix-p "*compilation" (buffer-name (process-buffer proc)))
                        (xterm-color-filter string) string)))
  (advice-add 'compilation-filter :around #'iensu--advice-compilation-filter)
  (add-hook 'compilation-mode-hook
            (lambda () (setq compilation-environment '("TERM=xterm-256color")))))

;; Install vterm for better terminal support
(use-package vterm
  :config
  (defun iensu/project-vterm ()
    "Open a vterm terminal at the current project root."
    (interactive)
    (let* ((default-directory (project-root (project-current t)))
           (vterm-buffer-name (project-prefixed-buffer-name "vterm"))
           (vterm-buffer (get-buffer vterm-buffer-name)))
      (if (and vterm-buffer (not current-prefix-arg))
          (pop-to-buffer vterm-buffer t)
        (vterm current-prefix-arg)))))

(use-package multi-vterm)

;;;;; Text editing tools

;; Spellcheck using flyspell
(use-package flyspell
  :bind (:map flyspell-mode-map ("C-:" . flyspell-popup-correct))
  :custom
  (ispell-program-name "aspell")
  (ispell-extra-args '("--sug-mode=ultra"))
  (ispell-list-command "--list")
  (ispell-dictionary "en_US")
  :config
  (unbind-key (kbd "C-.") 'flyspell-mode-map) ;; Using this binding for other stuff

  (defvar iensu--language-ring nil
    "Ispell language ring used to toggle current selected ispell dictionary")

  (let ((languages '("swedish" "en_US")))
    (setq iensu--language-ring (make-ring (length languages)))
    (dolist (elem languages) (ring-insert iensu--language-ring elem)))

  (defun iensu/cycle-ispell-dictionary ()
    "Cycle through the languages defined in `iensu--language-ring'."
    (interactive)
    (let ((language (ring-ref iensu--language-ring -1)))
      (ring-insert iensu--language-ring language)
      (ispell-change-dictionary language)
      (message (format "Switched to dictionary: %s" language))))
  (defalias 'sd #'iensu/cycle-ispell-dictionary
    "Switch spellchecking dictionary."))

(use-package flyspell-popup :after (flyspell))

;; Use synosaurus to look up synonyms
(use-package synosaurus
  :custom
  (synosaurus-backend 'synosaurus-backend-wordnet)
  (synosaurus-choose-method 'popup))

;; Emoji support because reasons...
(use-package emojify
  :custom
  (emojify-emojis-dir (expand-file-name ".local/emojis" user-emacs-directory)))

;; `visual-fill-column' makes it possible to visually wrap and center text which is good for
;; document-like editing.
(use-package visual-fill-column
  :config
  (setq-default visual-fill-column-center-text t)
  (let ((column-width 130))
    (setq-default visual-fill-column-width column-width)
    (setq fill-column column-width)))

(defun iensu/text-editing-mode-hook ()
  "Enables text editing tools such as spell checking and thesaurus support"
  (interactive)
  (flyspell-mode 1)
  (synosaurus-mode 1)
  (emojify-mode 1)
  (visual-line-mode 1)
  (visual-fill-column-mode 1))

(add-hook 'text-mode-hook #'iensu/text-editing-mode-hook)


;;;; Utility packages

(use-package marginalia
  :init
  (marginalia-mode)
  :config
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))

(use-package consult
  :bind
  (("C-c h"    . consult-history)
   ("C-x M-:"  . consult-complex-command)
   ("C-x b"    . consult-buffer)
   ("C-x 4 b"  . consult-buffer-other-window)
   ("C-x 5 b"  . consult-buffer-other-frame)
   ("M-y"      . consult-yank-pop)
   ("<help> a" . consult-apropos)
   ("M-g e"    . consult-compile-error)
   ("M-g g"    . consult-goto-line)
   ("M-g M-g"  . consult-goto-line)
   ("M-g o"    . consult-outline)
   ("M-g m"    . consult-mark)
   ("M-g k"    . consult-global-mark)
   ("M-g i"    . consult-imenu)
   ("M-g I"    . consult-project-imenu)
   ("H-s f"    . consult-find)
   ("H-s L"    . consult-locate)
   ("H-s g"    . consult-grep)
   ("H-s G"    . consult-git-grep)
   ("H-s r"    . consult-ripgrep)
   ("H-s l"    . consult-line)
   ("H-s m"    . consult-multi-occur)
   ("H-s k"    . consult-keep-lines)
   ("H-s u"    . consult-focus-lines)
   ("H-s e"    . consult-isearch)
   :map isearch-mode-map
   ("M-e"      . consult-isearch)
   ("M-s e"    . consult-isearch)
   ("M-s l"    . consult-line))
  :config
  (setq consult-narrow-key "<"))

(use-package embark
  :bind
  (("C-."   . embark-act)
   ("H-a"   . embark-act)
   ("H-e"   . embark-export)
   ("C-h B" . embark-bindings)))

(use-package embark-consult)

;; Install `hydra' with `pretty-hydra' which simplifies hydra definitions
(use-package hydra)
(use-package pretty-hydra :after (hydra))

;; Armor exported PGP-keys
(setq epa-armor t)

;; Password entry in minibuffer
(use-package pinentry
  :init
  (setq epa-pinentry-mode 'loopback)
  (pinentry-start))

(setq-default dired-listing-switches "-alGh --group-directories-first"
              dired-dwim-target t)
(when (executable-find "gls") ;; native OSX ls works differently then GNU ls
  (setq insert-directory-program "/usr/local/bin/gls"))


;;;; Navigation
;; This section adds packages which enables quick navigation and search.

;; Mark-ring tweaks
(setq mark-ring-max 6
      global-mark-ring-max 8)
;; Simplify jumping between local marks (C-u C-<space>, C-<space> * n)
(setq-default set-mark-command-repeat-pop t)

(use-package deadgrep)

(use-package wgrep
  :load-path (lambda () (expand-file-name "packages/wgrep" user-emacs-directory))
  :config
  (setq wgrep-auto-save-buffer t)
  (require 'wgrep-deadgrep))

;; Snippet expansion for less repetitive text editing
(use-package yasnippet
  :delight yas-minor-mode
  :init
  (yas-global-mode 1)
  (setq yas-snippet-dirs (add-to-list 'yas-snippet-dirs (expand-file-name "snippets" user-emacs-directory)))
  :config
  (add-hook 'snippet-mode-hook (lambda ()
                                 (setq mode-require-final-newline nil
                                       require-final-newline nil))))

;; When all else fails
(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; Speedbar for file navigation
(require 'speedbar)
(defun iensu/speedbar-reset-layout ()
  (setf (alist-get 'width speedbar-frame-parameters) 60)
  (setf (alist-get 'height speedbar-frame-parameters) 45)
  (setf (alist-get 'left speedbar-frame-parameters) 0)
  (setf (alist-get 'top speedbar-frame-parameters) 0))

(add-hook 'speedbar-after-create-hook #'iensu/speedbar-reset-layout)

(define-key speedbar-file-key-map (kbd "<tab>") #'speedbar-toggle-line-expansion)
(global-set-key (kbd "C-ä") #'speedbar)


;;;; Custom commands

(defun iensu/toggle-scratch-buffer ()
  "Based on a great idea from Eric Skoglund (https://github.com/EricIO/emacs-configuration/)."
  (interactive)
  (if (string-equal (buffer-name (current-buffer))
                    "*scratch*")
      (switch-to-buffer (other-buffer))
    (switch-to-buffer "*scratch*")))

(defun iensu/toggle-profiler ()
  "Starts or stops the profiler, displaying the report when stopped."
  (interactive)
  (if (profiler-running-p)
      (progn
        (profiler-stop)
        (profiler-report))
    (progn
      (profiler-reset)
      (profiler-start 'cpu+mem))))

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

(defun iensu/finish-item ()
  "Sets a `Finished' property on an org-mode item. The value is the current time as an inactive timestamp."
  (interactive)
  (org-set-property "Finished" (iensu--get-current-inactive-timestamp)))


;;;; Global key-bindings

(global-set-key (kbd "C-<backspace>") 'delete-indentation)
(global-set-key (kbd "C-h C-s") 'iensu/toggle-scratch-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-<backspace>") 'fixup-whitespace)
(global-set-key (kbd "M-i") 'imenu)
(global-set-key (kbd "M-o") 'occur)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-unset-key (kbd "C-z")) ;; Disable the suspend Emacs ikey-binding
(global-set-key (kbd "C-ö") 'window-toggle-side-windows)
(global-set-key (kbd "H-g") 'goto-line)

;; Trying out some of Steve Yegge's re-bindings
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\M-p" 'previous-line)
(global-set-key "\M-n" 'next-line)

;; `windmove' enables navigation using `<shift>-<direction>'. These bindings conflict `org-mode' so
;; we make `windmove' take precedence.
(windmove-default-keybindings)
(setq org-replace-disputed-keys t) ; This line needs to occur before `org-mode' is loaded.

;;;;; Global hydra

;; Setup a global hydra with keybindings I use very often.
(pretty-hydra-define iensu-hydra
  (:color teal :quit-key "q" :title "Global commands")
  ("Utilities"
   (("d"   iensu/duplicate-line                    "duplicate line" :exit nil)
    ("s"   deadgrep                                "search")
    ("t"   toggle-truncate-lines                   "truncate lines")
    ("u"   revert-buffer                           "reload buffer")
    ("D"   iensu/cycle-ispell-dictionary           "change dictionary")
    ("+"   (lambda () (interactive) (enlarge-window-horizontally 10)) "enlarge horizontally" :exit nil)
    ("?"   (lambda () (interactive) (enlarge-window 5))               "enlarge vertically" :exit nil)
    ("-"   (lambda () (interactive) (shrink-window-horizontally 10))  "shrink horizontally" :exit nil)
    ("_"   (lambda () (interactive) (shrink-window 5))                "shrink vertically" :exit nil))
   "Bookmarks"
   (("l"   list-bookmarks                  "list bookmarks")
    ("b"   bookmark-set                    "set bookmark"))
   "Misc"
   (("å"   iensu/treemacs                  "open treemacs view")
    ("P"   iensu/project-todo-list         "project todo list")
    ("p"   iensu/open-project-org-file     "open project notes file")
    ("ä"   iensu/promote-side-window       "promote side window"))
   "Hide/show"
   (("h h" hs-toggle-hiding                "toggle block visibility")
    ("h l" hs-hide-level                   "hide all blocks at same level")
    ("h a" hs-hide-all                     "hide all")
    ("h s" hs-show-all                     "show all"))))

(global-set-key (kbd "C-å") #'iensu-hydra/body)

;; Enhance explorability with by listing possible completions while doing key chords.
(use-package which-key :config (which-key-mode))

;;;;; macOS specific keybindings

;; Set command to act as `meta' (`M-') and disable the `option' key since that button is needed to
;; type various characters on a Swedish keyboard. Also make the right `option' key act as `hyper'
;; (`H-') to give us more keybindings to work with.
(setq mac-command-modifier 'meta
      mac-option-modifier 'none
      mac-right-option-modifier 'hyper)

;; An unfortunate workaround required when switching to an external keyboard.
(defun iensu/switch-left-and-right-option-keys ()
  "Switch left and right option keys.

     On some external keyboards the left and right Mac `option' keys are swapped,
     this command switches the keys so that they work as expected."
  (interactive)
  (let ((current-left  mac-option-modifier)
        (current-right mac-right-option-modifier))
    (setq mac-option-modifier       current-right
          mac-right-option-modifier current-left)))


;;;; Make Emacs prettier

(setq-default cursor-type 'box)
(global-prettify-symbols-mode 1)
(global-font-lock-mode 1)

;; Use dark mode on macOS.
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearence . dark))
(setq frame-title-format nil)

;; Use icons where applicable.
(use-package all-the-icons)

(use-package modus-themes
  :config
  (load-theme 'modus-operandi-tinted t))

;; Pimp tab-bar-mode
(use-package emacs
  :config
  (add-to-list 'tab-bar-format #'tab-bar-format-menu-bar)

  (set-face-attribute 'tab-bar nil
                      :family "Monospace"
                      :background nil)
  (set-face-attribute 'tab-bar-tab nil
                      :box `(:line-width (4 . 8) :color ,(face-attribute 'default :background)))
  (set-face-attribute 'tab-bar-tab-inactive nil
                      :box `(:line-width (4 . 8) :color ,(face-attribute 'default :background))
                      :background nil
                      :foreground "#969696")

  (setq tab-bar-close-button-show nil
        tab-bar-new-button-show nil
        tab-bar-separator "  ")
  (add-to-list 'desktop-path "~/.emacs.d/.local/"))


;;;; Version control

;; Make `magit' and other version control tools follow symlinks.
(setq vc-follow-symlinks t)

;; Use `magit' for a great `git' experience.
(use-package magit
  :bind (("C-x g" . magit-status))
  :custom
  (magit-bury-buffer-function 'quit-window)
  :config
  (when (executable-find "~/.nix-profile/bin/git") ; Speeds up git operations on macOS
    (setq magit-git-executable "~/.nix-profile/bin/git"))
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (setq magit-refresh-status-buffer nil))

(use-package forge
  :after magit)

;; `smerge-mode' is a merge conflict resolution tool which is great but unfortunately has awful
;; default keybindings. Here I define a hydra to make `smerge' easier to work with.
(use-package smerge-mode
  :bind (:map smerge-mode-map (("C-c ö" . smerge-mode-hydra/body)))
  :pretty-hydra
  ((:color teal :quit-key "q" :title "Smerge - Git conflicts")
   ("Resolving"
    (("RET" smerge-keep-current      "Keep current"          :exit nil)
     ("l"   smerge-keep-lower        "Keep lower"            :exit nil)
     ("u"   smerge-keep-upper        "Keep upper"            :exit nil)
     ("b"   smerge-keep-base         "Keep base"             :exit nil)
     ("C"   smerge-combine-with-next "Combine with next")
     ("a"   smerge-keep-all          "Keep all"              :exit nil)
     ("r"   smerge-resolve           "Resolve"))
    "Navigation"
    (("n"   smerge-next              "Next conflict"         :exit nil)
     ("p"   smerge-prev              "Previous conflict"     :exit nil)
     ("R"   smerge-refine            "Highlight differences" :exit nil))
    "Misc"
    (("E"   smerge-ediff             "Open in Ediff")))))


;;;; Project management

;; Settings which help with handling and navigating projects.
(defun iensu/project-save ()
  "Save current project to `project-list-file'."
  (interactive)
  (project-remember-project (project-current)))

(defun iensu/project-remove ()
  "Remove current project from `project-list-file'."
  (interactive)
  (project-remove-known-project (project-root (project-current))))

(defun iensu/project-ripgrep ()
  "Run ripgrep in the current project."
  (interactive)
  (consult-ripgrep (project-root (project-current))))

(use-package project
  :bind
  (("C-c p" . project-hydra/body))
  :pretty-hydra
  ((:color teal :quit-key "q" :title "Project management")
   ("Project"
    (("p" project-switch-project "open project")
     ("k" project-kill-buffers "close project")
     ("a" iensu/project-save "remember project")
     ("A" iensu/project-remove "forget project")
     ("v" iensu/project-vterm "vterm"))
    "Files & Buffers"
    (("f" project-find-file "open project file")
     ("o" iensu/open-project-org-file "open project org file")
     ("T" iensu/project-todo-list "open project TODO list"))
    "Search"
    (("s" iensu/project-ripgrep "search")
     ("r" project-query-replace-regexp "query replace"))))
  :config
  (setq project-list-file (expand-file-name "projects"
                                            (concat user-emacs-directory ".local/")))

  ;; Handle projects which are not version controlled
  (defun iensu--locate-non-vc-project (dir)
    "Locate project root based on the existence of .project file.
Falls back to looking for .projectile for compatibility reasons."
    (let ((root (or (locate-dominating-file dir ".project")
                    (locate-dominating-file dir ".projectile"))))
      (and root (cons 'non-vc root))))
  (add-to-list 'project-find-functions #'iensu--locate-non-vc-project)
  (cl-defmethod project-root ((project (head non-vc)))
    "Handle `non-vc' projects, i.e. projects which are not version controlled."
    (cdr project)))

(use-package treemacs
  :init
  (defun iensu/treemacs ()
    "Open Treemacs and query for the active workspace."
    (interactive)
    (unless (treemacs-get-local-window)
      (treemacs))
    (treemacs-switch-workspace nil)))

(use-package treemacs-magit :after treemacs magit)

;; Force all ediff windows to be in the same frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;;;; Project-based TODO lists

;; Create a TODO list based on TODO items in a project's `.project-notes.org' file. The
;; `org-agenda-files' variable is temporarily set the only the project notes file and then reverted
;; back to its previous value upon closing the TODO list buffer.

(defvar iensu--project-agenda-buffer-name "*Project Agenda*")

(defun iensu--org-capture-project-notes-file ()
  (concat (locate-dominating-file (buffer-file-name) ".git") ".project-notes.org"))

(defun iensu/create-project-notes-file ()
  "Creates a note file somewhere in `org-directory' and links it to the current directory as `.project-notes.org.'"
  (interactive)
  (let* ((versioned-dir (locate-dominating-file (buffer-file-name)
                                                ".git"))
         (project-dir (expand-file-name (or versioned-dir
                                            (file-name-directory (buffer-file-name)))))
         (project-name (car (last (remove-if (lambda (x) (string-equal ""
                                                                       x))
                                             (split-string project-dir
                                                           "/")))))
         (notes-link (concat project-dir ".project-notes.org"))
         (notes-file-name (concat project-name ".org"))
         (notes-dir (expand-file-name (read-directory-name (format "Where to save %s? "
                                                                   notes-file-name)
                                                           (concat (file-name-as-directory org-directory)
                                                                   "projects"))))
         (notes-file (concat notes-dir notes-file-name)))
    (make-empty-file notes-file)
    (make-symbolic-link notes-file notes-link)
    (find-file notes-link)
    (iensu/refresh-agenda-files)))

(defun iensu/refresh-agenda-files ()
  (interactive)
  (setq-default org-agenda-files
                (append (directory-files iensu-org-dir
                                         :full-path
                                         "\\.org$")
                        (directory-files-recursively (concat iensu-org-dir "/projects")
                                                     "\\.org$")
                        (directory-files-recursively (concat iensu-org-dir "/calendars")
                                                     "\\.org$"))))

(defun iensu/project-todo-list ()
  (interactive)
  (let ((project-notes-file (expand-file-name ".project-notes.org"
                                              (project-root (project-current)))))
    (if (file-exists-p project-notes-file)
        (progn
          (setq org-agenda-files `(,project-notes-file))
          (org-todo-list)
          (rename-buffer iensu--project-agenda-buffer-name 'unique))
      (message "Could not locate any project notes file"))))

(defun iensu/reset-org-agenda-files ()
  (interactive)
  (when (string-equal iensu--project-agenda-buffer-name
                      (buffer-name (current-buffer)))
    (setq org-agenda-files iensu-org-agenda-files)))

(defun iensu/open-project-org-file ()
  (interactive)
  (find-file (iensu--org-capture-project-notes-file)))

;; Reset org-agenda-files when the project TODO list buffer is closed
(add-hook 'kill-buffer-hook #'iensu/reset-org-agenda-files)

;; Add some org-capture templates for project notes.
(iensu-add-to-list 'iensu-org-capture-templates
                   `("m" "Project note" entry (file+headline iensu--org-capture-project-notes-file "Notes")
                     ,(concat "* %^{Title}\n"
                              "%U\n\n"
                              "%?")
                     :empty-lines 1)

                   `("n" "Project note with link" entry (file+headline iensu--org-capture-project-notes-file "Notes")
                     ,(concat "* %^{Title}\n"
                              "%U\n\n"
                              "Link: %a\n\n"
                              "%?")
                     :empty-lines 1)

                   `("N" "Project note with link + code quote" entry (file+headline iensu--org-capture-project-notes-file "Notes")
                     ,(concat "* %^{Title}\n"
                              "%U\n\n"
                              "Link: %a\n\n"
                              "#+begin_src %^{Language}\n"
                              "%i\n"
                              "#+end_src"
                              "\n\n"
                              "%?")
                     :empty-lines 1))


;;;; IDE features

;; Highlight TODOs in programming buffers
(use-package hl-todo :hook ((prog-mode . hl-todo-mode)))

;; Use tree-sitter for syntax highlighting where availaible
(use-package tree-sitter
  :config
  (global-tree-sitter-mode))
(use-package tree-sitter-langs)

;;;;; Autocompletion and intellisense

 ;; Corfu for completions
(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.1)
  (corfu-quit-at-boundary 'separator)
  (corfu-echo-documentation 0.25)
  (corfu-preview-current 'insert)
  (corfu-preselect-first t)
  :init
  (global-corfu-mode 1)

  (let ((corfu-extension-dir (concat straight-base-dir "straight/" straight-build-dir "/corfu/extensions/")))
    (dolist (ext '("corfu-echo" "corfu-history"))
      (load-file (concat corfu-extension-dir ext ".el"))))

  (corfu-echo-mode 1)
  (corfu-history-mode 1))

(use-package cape
  :bind (("H-c p" . completion-at-point)
         ("H-c t" . complete-tag)
         ("H-c d" . cape-dabbrev)
         ("H-c h" . cape-history)
         ("H-c f" . cape-file)
         ("H-c k" . cape-keyword)
         ("H-c s" . cape-symbol)
         ("H-c a" . cape-abbrev)
         ("H-c i" . cape-ispell)
         ("H-c l" . cape-line)
         ("H-c w" . cape-dict)
         ("H-c \\" . cape-tex)
         ("H-c _" . cape-tex)
         ("H-c ^" . cape-tex)
         ("H-c &" . cape-sgml)
         ("H-c r" . cape-rfc1345))
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))


(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-cycle t))

(use-package savehist
  :init
  (savehist-mode 1))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; https://github.com/minad/vertico#configuration
(use-package emacs
  :init
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (setq read-extended-command-predicate #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))


(add-hook 'prog-mode-hook (lambda ()
                            (flymake-mode 1)))
(use-package flymake-eslint)

(pretty-hydra-define prog-mode-hydra
  (:color teal :quit-key "q" :title "Programming")
  ("Exploration"
    (("l" xref-find-references "list references")
     ("d" eldoc-doc-buffer "describe symbol")
     ("e" flymake-show-buffer-diagnostics "list buffer errors")
     ("å" flymake-goto-previous-error "goto previous error in buffer")
     ("ä" flymake-goto-next-error "goto next error in buffer ")
     ("E" flymake-show-project-diagnostics "list workspace errors"))))

(define-key prog-mode-map (kbd "C-c l") 'prog-mode-hydra/body)
(define-key prog-mode-map (kbd "M-<RET>") 'default-indent-new-line)

(use-package eglot
  :bind (:map eglot-mode-map
              ("C-c l" . eglot-hydra/body))
  :pretty-hydra
  ((:title "Eglot" :quit-key "q" :color teal)
   ("Exploration"
    (("l" xref-find-references "list references")
     ("d" eldoc-doc-buffer "describe symbol")
     ("e" flymake-show-buffer-diagnostics "list buffer errors")
     ("å" flymake-goto-previous-error "goto previous error in buffer")
     ("ä" flymake-goto-next-error "goto next error in buffer ")
     ("E" flymake-show-project-diagnostics "list workspace errors"))
    "Refactoring"
    (("a" eglot-code-actions "execute code action")
     ("n" eglot-rename "rename symbol")
     ("i" eglot-code-actions-organize-imports "organize imports")
     ("f" eglot-format-buffer "format buffer"))
    "Misc"
    (("w" eglot-reconnect "Reconnect to LSP server")))))

(use-package lsp-mode
  :bind (:map lsp-mode-map
              ("C-c l" . lsp-mode-hydra/body))
  :pretty-hydra
  ((:title "LSP" :quit-key "q" :color teal)
   ("Exploration"
    (("l" xref-find-references "list references")
     ("d" eldoc-doc-buffer "describe symbol")
     ("e" flymake-show-buffer-diagnostics "list buffer errors")
     ("å" flymake-goto-previous-error "goto previous error in buffer")
     ("ä" flymake-goto-next-error "goto next error in buffer ")
     ("E" flymake-show-project-diagnostics "list workspace errors"))
    "Refactoring"
    (("a" lsp-execute-code-action "execute code action")
     ("n" lsp-rename "rename symbol")
     ("i" lso-organize-imports "organize imports")
     ("f" lsp-format-buffer "format buffer"))
    "Misc"
    (("w" lsp-workspace-restart "Reconnect to LSP server")))))

(use-package lsp-ui :commands lsp-ui-mode)

;; Autoformatting
(use-package prettier-js)

;; HTTP requests
(use-package restclient
  :mode (("\\.rest$" . restclient-mode)
         ("\\.restclient$" . restclient-mode)
         ("\\.http$" . restclient-mode)))

(use-package direnv
  :config
  (direnv-mode)
  ;; Handle .direnv as shell file
  (add-to-list 'auto-mode-alist '("\\.envrc$" . sh-mode)))

(global-auto-revert-mode 1)


;;;; Window buffer management
(defun iensu--should-display-to-right (buffer-name _action)
  (let ((mode (buffer-local-value 'major-mode (get-buffer buffer-name))))
    (or (member mode '())
        (string-match "^\\*HTTP Response\\*" buffer-name))))

(setq display-buffer-alist
      '((iensu--should-display-to-right
         (display-buffer-reuse-window
          display-buffer-reuse-mode-window
          display-buffer-in-side-window)
         (side . right)
         (window-width . 70)
         (quit-restore ('window 'window nil nil)))))

(defun iensu/promote-side-window ()
  "Promotes a side window to a main window so it can be manipulated more easily."
  (interactive)
  (let ((buf (current-buffer)))
    (when (window-at-side-p (get-buffer-window buf))
      (display-buffer-use-some-window buf '())
      (delete-window))))


;;;; Load features

(dolist (feature iensu-enabled-features-alist)
  (load-file (expand-file-name (concat "features/" feature ".el")
                               user-emacs-directory)))
;; Use Denote for note taking
(use-package denote
  :straight (:source gnu-elpa-mirror)
  :config
  (setq denote-directory iensu-denote-dir)

  (add-to-list 'denote-file-types
               '(org-gpg :extension ".org.gpg"
                         :date-function denote-date-org-timestamp
                         :front-matter denote-org-front-matter
                         :title-key-regexp "^#\\+title\\s-*:"
                         :title-value-function identity
                         :title-value-reverse-function denote-trim-whitespace
                         :keywords-key-regexp "^#\\+filetags\\s-*:"
                         :keywords-value-function denote-format-keywords-for-org-front-matter
                         :keywords-value-reverse-function denote-extract-keywords-from-front-matter
                         :link denote-org-link-format
                         :link-in-context-regexp denote-org-link-in-context-regexp))

  (defalias 'dg #'denote "Create a general Denote note")

  (defun iensu/denote-journal ()
    "Create an entry tagged 'journal' with the date as its title."
    (interactive)
    (denote
     (format-time-string "%A %e %B %Y")
     '("journal")
     'org-gpg
     (concat denote-directory "/journal")))
  (defalias 'dj #'iensu/denote-journal)

  (defun iensu/denote-work ()
    "Create an entry in a 'work' subdirectory."
    (interactive)
    (denote
     (denote-title-prompt)
     (append (denote-keywords-prompt) '("work"))
     'org-gpg
     (concat denote-directory "/work")))
  (defalias 'dw #'iensu/denote-work))

;; Load additional local feature configurations
(let ((feature-conf (expand-file-name "local-feature-settings.el" user-emacs-directory)))
  (when (file-exists-p feature-conf)
    (load-file feature-conf)))
