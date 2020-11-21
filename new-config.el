;;; Code:

;;;; Package installation and management

;; This section initializes `package' with a set of archives and installs `use-package' which is
;; used to install other package dependencies.

(require 'package)

(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(unless (package-installed-p 'use-package) ; install use-package if it's not already installed
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure))


;;;; System local configuration

;; This section handles configurations that are local to the system and thus should not be under
;; version control, for instance credentials and system specific feature flags.

(load-file (expand-file-name "local-settings.el"
                             user-emacs-directory))


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

;; Make sure that buffer names become unique when opening multiple files of the same name.
(setq-default frame-title-format "%b (%f)"
              uniquify-buffer-name-style 'post-forward
              uniquify-separator ":")a

;; Ask before killing Emacs.
(setq confirm-kill-emacs 'y-or-n-p)

;; Put any Emacs generated customizations into ./custom.el instead of ./init.el.
(setq custom-file (expand-file-name "custom.el"
                                    user-emacs-directory))

;; Improve performance by increasing the garbage collector threshold and max LISP evaluation depth.
(setq gc-cons-threshold 100000000
      max-lisp-eval-depth 2000)

;; Move backups and auto-saves to ~/.emacs.d/.local/.saves.
(setq create-lockfiles nil
      backup-directory-alist `(("." . ,(expand-file-name ".local/.saves/" user-emacs-directory)))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6)

;; Keep some files in ~/.emacs.d/.local to avoid cluttering the configuration root directory.
(setq url-configuration-directory (expand-file-name ".local/uri/" user-emacs-directory)
      image-dired-dir (expand-file-name ".local/image-dired-thumbnails/" user-emacs-directory)
      bookmark-default-file (expand-file-name ".local/bookmarks" user-emacs-directory)
      tramp-auto-save-directory (expand-file-name ".local/tramp-autosaves/" user-emacs-directory))

;; Setup authentication file and pinentry for entering passwords.
(setq auth-sources '("~/.authinfo.gpg"
                     "~/.netrc"))

;; Auto scroll through output in compilation buffers.
(setq compilation-scroll-output t)


;;;; Editor default settings

;; This section changes the default text editing behavior of Emacs so it is more inline with my
;; expectations.

;; Use spaces instead of tabs.
(setq-default indent-tabs-mode nil
              tab-width 2)

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

;;;;; Programming mode defaults

(defun iensu--prog-mode-hook ()
  "Defaults for programming modes"
  (subword-mode 1) ; delimit words at camelCase boundries
  (eldoc-mode 1) ; display documentation in minibuffer
  (show-paren-mode 1) ; highlight matching parentheses
  (setq-default show-paren-when-point-in-periphery t
                show-paren-when-point-inside-paren t)
  (hs-minor-mode 1) ; hide-show code and comment blocks
  (outline-minor-mode 1)) ; Navigate by outlines

(add-hook 'prog-mode-hook #'iensu--prog-mode-hook)


;;;; Utility packages

;;;;; Hydra

;; Install `hydra' with `pretty-hydra' which simplifies hydra definitions
(use-package hydra)
(use-package pretty-hydra :after (hydra))


;;;; Global keybindings

(global-set-key (kbd "C-<backspace>") 'delete-indentation)
(global-set-key (kbd "C-h C-s") 'iensu/toggle-scratch-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-<backspace>") 'fixup-whitespace)
(global-set-key (kbd "M-i") 'imenu)
(global-set-key (kbd "M-o") 'occur)

;; `windmove' enables navigation using `<shift>-<direction>'. These bindings conflict `org-mode' so
;; we make `windmove' take precedence.
(windmove-default-keybindings)
(setq org-replace-disputed-keys t) ; This line needs to occur before `org-mode' is loaded.

;;;;; Global hydra

;; Setup a global hydra with keybindings I use very often.
(pretty-hydra-define iensu-hydra
  (:color teal :quit-key "q" :title "Global commands")
  ("Email"
   (("e u" mu4u-update-index               "update" :exit nil)
    ("e e" mu4e                            "open email")
    ("e c" mu4e-compose-new                "write email")
    ("e s" mu4e-headers-search             "search email"))
   "Elfeed"
   (("f f" elfeed)
    ("f u" elfeed-update))
   "Org clock"
   (("c c" org-clock-in                    "start clock")
    ("c r" org-clock-in-last               "resume clock")
    ("c s" org-clock-out                   "stop clock")
    ("c g" org-clock-goto                  "goto clocked task"))
   "Utilities"
   (; ("d"   iensu/duplicate-line            "duplicate line" :exit nil)
    ("s"   deadgrep                        "search")
    ("t"   toggle-truncate-lines           "truncate lines")
    ("u"   revert-buffer                   "reload buffer")
    ; ("l"   iensu/cycle-ispell-dictionary   "change dictionary"))
   "Misc"
   (("P"   iensu/project-todo-list         "project todo list")
    ("i"   iensu/open-init-file            "open emacs config")
    ("9"   iensu/refresh-work-calendar     "update calendar")
    ("+"   enlarge-window-horizontally     "enlarge window" :exit nil)
    ("-"   shrink-window-horizontally      "shrink window" :exit nil))
   "Hide/show"
   (("h h" hs-toggle-hiding "Toggle block visibility")
    ("h l" hs-hide-level "Hide all blocks at same level")
    ("h a" hs-hide-all "Hide all")
    ("h s" hs-show-all "Show all"))))

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

(setq-default cursor-type '(bar . 2))
(global-prettify-symbols-mode 1)
(global-font-lock-mode 1)

;; Use dark mode on macOS.
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearence . dark))

;; Use different colored parentheses based on scope.
(use-package rainbow-delimiters
  :hook (prog-mode . #'rainbow-delimiters-mode))

;; Use icons where applicable.
(use-package all-the-icons)

;; Use a clean mode line
(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; Set theme
(use-package modus-vivendi-theme
    :config
    (load-theme 'modus-vivendi t)
    (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
    (set-face-attribute 'font-lock-comment-delimiter-face nil :slant 'italic)
    (set-face-attribute 'default nil :font "Fira Code-13")
    (set-face-attribute 'fixed-pitch nil :font "Fira Code-13")
    (set-face-attribute 'variable-pitch nil :font "Cantarell-14"))


;;;; Version control

;; Make `magit' and other version control tools follow symlinks.
(setq vc-follow-symlinks t)

;; Use `magit' for a great `git' experience.
(use-package magit
    :bind (("C-x g" . magit-status))
    :custom
    (magit-bury-buffer-function 'quit-window)
    :config
    (when (executable-find "/usr/bin/git") ; Speeds up git operations on macOS
      (setq magit-git-executable "/usr/bin/git"))
    (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; `smerge-mode' is a merge conflict resolution tool which is great but unfortunately has awful
;; default keybindings. Here I define a hydra to make `smerge' easier to work with.
(use-package smerge-mode
  :ensure nil
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



;;;; Org mode

;;;;; Agenda

(setq calendar-week-start-day 1) ; The week starts on Monday.


;;;; Emacs server

;; The Emacs server keeps the Emacs instance running in the background so opening files in Emacs
;; will be snappy.
(unless (server-running-p)
  (server-start))


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
