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
  (setq use-package-always-ensure t))


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

(defvar iensu-gcal-client-id nil
  "Client ID for Gmail integration.")

(defvar iensu-gcal-client-secret nil
  "Client secret for Gmail integration.")

(defvar iensu-org-capture-templates nil
  "Capture templates to be used by Org mode.")

(defvar iensu-email-directory "~/Mail"
  "Path to email directory.")

(defvar iensu-email-mu4e-package-path "/usr/local/share/emacs/site-lisp/mu/mu4e"
  "Path to mu4e Emacs package.")

(defvar iensu-email-mu-binary-path "/usr/local/bin/mu"
  "Path to the mu binary.")

(defvar iensu-enabled-features-alist nil
  "Locally enabled features. Available features are stored in the `features/' directory.")


;; Load settings
(load-file (expand-file-name "local-settings.el"
                             user-emacs-directory))


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

;; Improve performance by increasing the garbage collector threshold and max LISP evaluation depth.
(setq gc-cons-threshold 100000000
      max-lisp-eval-depth 2000)

;; Move backups and auto-saves
(setq create-lockfiles nil
      backup-directory-alist `(("." . ,(expand-file-name ".local/backups/" user-emacs-directory)))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6

      auto-save-list-file-name (expand-file-name ".local/auto-saves-list" user-emacs-directory))

;; Enable autosaves
(auto-save-mode 1)

;; Remember recent files
(use-package recentf
  :ensure nil
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

;;;;; Text editing tools

;; Spellcheck using flyspell
(use-package flyspell
  :ensure nil
  :bind (:map flyspell-mode-map ("C-:" . flyspell-popup-correct))
  :custom
  (ispell-program-name "/usr/local/bin/aspell")
  (ispell-extra-args '("--sug-mode=ultra"))
  (ispell-list-command "--list")
  (ispell-dictionary "en_US")
  :config
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
      (message (format "Switched to dictionary: %s" language)))))
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
  (setq-default visual-fill-column-width 110)
  (setq-default visual-fill-column-center-text t))

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

;; Install `hydra' with `pretty-hydra' which simplifies hydra definitions
(use-package hydra)
(use-package pretty-hydra :after (hydra))

;; Make system path variables accessible in Emacs
(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-check-startup-files nil)
  :init
  (exec-path-from-shell-initialize))

;; Password entry in minibuffer
(use-package pinentry
  :init
  (setq epa-pinentry-mode 'loopback)
  (pinentry-start))

;; Improved file browsing
(use-package dired+
  :load-path (lambda () (expand-file-name "packages" user-emacs-directory))
  :custom
  (dired-listing-switches "-alGh --group-directories-first")
  (dired-dwim-target t)
  :config
  (when (executable-find "gls") ;; native OSX ls works differently then GNU ls
    (setq insert-directory-program "/usr/local/bin/gls")))

;;;; Navigation

;; This section adds packages which enables quick navigation and search.

(use-package deadgrep)

;; `counsel', `ivy' and `swiper' constitute a very useful completion framework.
(use-package counsel
  :delight ivy-mode
  :init
  (ivy-mode 1)
  :bind (("M-x"     . counsel-M-x)
         ("C-x C-f"	. counsel-find-file)
         ("C-x C-r" . counsel-recentf)
         ("C-c k"   . counsel-ag)
         ("C-x b"   . ivy-switch-buffer)
         ("M-y"     . counsel-yank-pop)
         ("C-s"     . swiper-isearch)
         :map ivy-minibuffer-map
         ("M-y"     . ivy-next-line))
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-use-selectable-prompt t)
  (ivy-count-format "(%d/%d) ")
  (ivy-magic-slash-non-match-action 'ivy-magic-non-match-create)
  (counsel-ag-base-command "ag --nocolor --nogroup --hidden %s")
  (ivy-display-style 'fancy)
  (ivy-re-builders-alist '((swiper . ivy--regex-plus)
                           (swiper-isearch . ivy--regex-plus)
                           (counsel-find-file . ivy--regex-plus)
                           (counsel-projectile-find-file . ivy--regex-plus)
                           (t . ivy--regex-plus))))

(use-package ivy-rich :config (ivy-rich-mode 1)) ; Add documentation to ivy results

;; `prescient' ranks search candidates by most recent.
(use-package prescient :config (prescient-persist-mode 1))
(use-package ivy-prescient
  :config
  (ivy-prescient-mode 1)
  (setq ivy-prescient-enable-sorting t)
  (setq ivy-prescient-enable-filtering t))
(use-package company-prescient
  :config
  (company-prescient-mode 1))

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
  ("Utilities"
   (("d"   iensu/duplicate-line            "duplicate line" :exit nil)
    ("s"   deadgrep                        "search")
    ("t"   toggle-truncate-lines           "truncate lines")
    ("u"   revert-buffer                   "reload buffer")
    ("D"   iensu/cycle-ispell-dictionary   "change dictionary"))
   "Bookmarks"
   (("l"   list-bookmarks                  "list bookmarks")
    ("b"   bookmark-set                    "set bookmark"))
   "Misc"
   (("P"   iensu/project-todo-list         "project todo list")
    ("p"   iensu/open-project-org-file     "open project notes file")
    ("i"   list-bookmarks                  "list bookmarks")
    ("+"   enlarge-window-horizontally     "enlarge window" :exit nil)
    ("-"   shrink-window-horizontally      "shrink window" :exit nil))
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

(setq-default cursor-type '(bar . 2))
(global-prettify-symbols-mode 1)
(global-font-lock-mode 1)

;; Use dark mode on macOS.
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearence . dark))

;; Use different colored parentheses based on scope.
;;(use-package rainbow-delimiters
;;  :hook (prog-mode . #'rainbow-delimiters-mode))

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
  (set-face-attribute 'font-lock-comment-delimiter-face nil :slant 'italic))


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


;;;; Project management

;; Settings which help with handling and navigating projects.

(use-package projectile
  :bind
  (("C-c p" . projectile-hydra/body))
  :custom
  (projectile-completion-system 'ivy)
  (projectile-cache-file (expand-file-name ".local/projectile.cache" user-emacs-directory))
  (projectile-known-projects-file (expand-file-name ".local/projectile-bookmarks.eld" user-emacs-directory))
  (projectile-git-submodule-command nil)
  (projectile-sort-order 'access-time)
  (projectile-globally-ignored-files '("TAGS" ".DS_Store" ".projectile"))
  :pretty-hydra
  ((:color teal :quit-key "q" :title "Project")
   ("Project"
    (("p" counsel-projectile-switch-project "open project")
     ("k" projectile-kill-buffers "close project")
     ("t" projectile-test-project "test project" :exit t)
     ("c" projectile-compile-project "compile project" :exit t))
    "Files & Buffers"
    (("f" counsel-projectile-find-file "open project file")
     ("o" iensu/open-project-org-file "open project org file")
     ("T" iensu/project-todo-list "open project TODO list")
     ("b" counsel-projectile-switch-to-buffer "open project buffer")
     ("S" projectile-save-buffers "save project buffers"))
    "Search"
    (("s" projectile-ripgrep "search")
     ("r" projectile-replace "replace literal")
     ("R" projectile-replace-regexp "replace regex"))))
  :config
  (projectile-global-mode)
  (projectile-register-project-type 'node-npm '("package.json")
                                    :compile "npm run build"
                                    :test "npm test")
  (projectile-register-project-type 'rust-cargo '("cargo.toml")
                                    :compile "cargo check"
                                    :test "cargo test"
                                    :run "cargo run")
  (projectile-register-project-type 'java-maven '("pom.xml")
                                    :compile "mvn compile"
                                    :test "mvn test"))

(use-package counsel-projectile :init (counsel-projectile-mode 1))

(use-package ibuffer-projectile :after (projectile)
  :hook
  (ibuffer-mode . (lambda ()
                    (ibuffer-projectile-set-filter-groups)
                    (unless (eq ibuffer-sorting-mode 'alphabetic)
                      (ibuffer-do-sort-by-alphabetic)))))

;; `treemacs' for a visual project tree structure.
(use-package treemacs
  :defer t
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)
        ("C-x t w"   . treemacs-switch-workspace)))
(use-package treemacs-magit :after treemacs magit)
(use-package treemacs-projectile :after treemacs projectile)

;;;;; Project-based TODO lists

;; Create a TODO list based on TODO items in a project's `.project-notes.org' file. The
;; `org-agenda-files' variable is temporarily set the only the project notes file and then reverted
;; back to its previous value upon closing the TODO list buffer.

(defvar iensu--project-agenda-buffer-name "*Project Agenda*")

(defun iensu--org-capture-project-notes-file ()
  (concat (projectile-project-root) ".project-notes.org"))

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
    (find-file notes-link)))

(defun iensu/project-todo-list ()
  (interactive)
  (let ((project-notes-file (expand-file-name ".project-notes.org"
                                              (projectile-project-root))))
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

;;;;; Autocompletion and intellisense

;; Company as a completion frontend
(use-package company
  :init (global-company-mode)
  :bind (:map company-active-map (("C-n" . company-select-next)
                                  ("C-p" . company-select-previous)
                                  ("C-d" . company-show-doc-buffer)
                                  ("M-." . company-show-location)))
  :config
  (setq company-idle-delay 0.3)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  (setq company-auto-complete t)
  (setq company-tooltip-align-annotations t)
  (setq company-dabbrev-downcase nil)
  (setq company-auto-complete-chars nil)
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (add-to-list 'company-backends 'company-elisp)))
  (eval-after-load 'company (company-quickhelp-mode 1)))

(use-package company-quickhelp
  :bind (:map company-active-map
              ("M-h" . company-quickhelp-manual-begin))
  :config
  (setq company-quickhelp-delay 1))

;; Flycheck for on the fly error reporting
(use-package flycheck
  :init
  (global-flycheck-mode t)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
(use-package flycheck-popup-tip)

;; LSP for intellisense
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :bind (:map lsp-mode-map
              ("C-c l" . lsp-mode-hydra/body))
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  :config
  (setq lsp-auto-guess-root nil)

  :pretty-hydra
  ((:title "LSP" :quit-key "q" :color teal)
   ("Exploration"
    (("l" lsp-find-references "list references")
     ("s" lsp-ivy-workspace-symbol "search symbol in workspace")
     ("d" lsp-describe-thing-at-point "describe")
     ("e" flycheck-list-errors "list buffer errors")
     ("å" flycheck-previous-error "goto previous error in buffer")
     ("ä" flycheck-next-error "goto next error in buffer ")
     ("E" lsp-treemacs-errors-list "list workspace errors")
     ("T" lsp-goto-type-definition "find type definition"))
    "Refactoring"
    (("a" lsp-execute-code-action "execute code action")
     ("n" lsp-rename "rename symbol")
     ("i" lsp-organize-imports "organize imports")
     ("f" lsp-format-buffer "format buffer"))
    "Misc"
    (("w" lsp-restart-workspace "restart LSP server")))))

;; `lsp-ui' enables in buffer documentation popups etc.
(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (lsp-ui-sideline-mode 1)
  (setq lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-code-actions nil
        lsp-ui-sideline-show-symbol nil
        lsp-ui-sideline-show-hover nil)
  (lsp-ui-doc-mode 1))

(use-package company-lsp :commands company-lsp)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

;; Autoformatting
(use-package prettier-js)

;; HTTP requests
(use-package restclient :mode ("\\.rest$" "\\.restclient$"))
;; HTTP requests in Org files
(use-package ob-restclient
  :after (org)
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((restclient . t))))

;; Handle .direnv as shell file
(add-to-list 'auto-mode-alist '("\\.envrc$" . sh-mode))


;;;; Load features

(dolist (feature iensu-enabled-features-alist)
  (load-file (expand-file-name (concat "features/" feature ".el")
                               user-emacs-directory)))

;; Load additional local feature configurations
(let ((feature-conf (expand-file-name "local-feature-settings.el" user-emacs-directory)))
  (when (file-exists-p feature-conf)
    (load-file feature-conf)))
