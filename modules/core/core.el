;;; core/core.el --- Core settings

;;; Code:

(fset 'yes-or-no-p 'y-or-n-p)

(setq default-directory "~/"
      custom-file (iensu--config-file "custom.el")

      inhibit-startup-message t
      ring-bell-function 'ignore
      initial-scratch-message ";;; Scratch\n\n"
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
      recentf-save-file (iensu--config-file ".local/recentf")
      image-dired-dir (iensu--config-file ".local/image-dired")
      bookmark-default-file (iensu--config-file ".local/bookmarks")
      tramp-auto-save-directory (iensu--config-file ".local/tramp")

      next-screen-context-lines 20)

;; On MacOS/OSX remember to disable the built in dictionary lookup command
;; by running the following command followed by a restart
;; defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 70 '<dict><key>enabled</key><false/></dict>'
(setq mac-option-modifier 'none
      mac-command-modifier 'meta
      mac-function-modifier 'hyper
      mac-right-option-modifier 'hyper)

(setq-default browse-url-browser-function 'eww-browse-url)

(defun iensu--eww-open-new-window (url &optional new-window)
  (when (not (string= "*eww*" (buffer-name)))
                         (cond
                          ((>= (window-width) 130) (progn (split-window-horizontally)
                                                          (other-window 1)))
                          ((>= (count-windows) 2) (other-window 1)))))

(advice-add 'eww-browse-url :before 'iensu--eww-open-new-window)

;; Need to setup identity using `gpg --gen-key` before using gpg
;; on Mac install pinentry-mac from homebrew
;; https://www.gnupg.org/software/pinentry/index.html
(setq auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc")
      epa-pinentry-mode 'loopback)

(use-package pinentry
  :init
  (pinentry-start))

(setq-default apropos-do-all t)

(use-package conf-mode
  :init
  (iensu-add-auto-mode 'conf-mode "\\.env$"))

(use-package recentf
  :config
  (add-to-list 'recentf-exclude (iensu--config-file ".local/\.*")))

(use-package saveplace
  :init
  (save-place-mode 1)
  (setq save-place-file (iensu--config-file ".local/.emacs-places")))

(when (memq window-system '(max ns))
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

;; Start Emacs server, which enables quick emacsclient access

(unless (server-running-p)
  (server-start))
