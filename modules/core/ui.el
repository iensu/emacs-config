;;; modules/core/ui.el --- UI

;;; Code:

(use-package zerodark-theme
  :ensure t
  :config
  (zerodark-setup-modeline-format)
  (set-face-attribute 'default nil
                      :font "Fira Mono"
                      :height 140)
  (add-hook 'org-mode-hook
            (lambda ()
              (set-face-attribute 'org-hide nil
                                  :foreground (face-attribute 'default :background))))
  (eval-after-load "linum"
    '(set-face-attribute 'linum nil
                         :font "Fira Mono"
                         :height 140
                         :italic nil
                         :weight 'light)))

(use-package all-the-icons
  :ensure t
  :defer nil)

(use-package all-the-icons-dired
  :ensure t
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package emojify
  :ensure t
  :init
  (add-hook 'text-mode-hook 'emojify-mode)
  (add-hook 'org-mode-hook 'emojify-mode)
  (setq emojify-emojis-dir (iensu/-config-file ".local/emojis")))

(use-package beacon
  :ensure t
  :diminish beacon-mode
  :config
  (beacon-mode 1))

(use-package diminish
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "ελ")))
  (add-hook 'lisp-interaction-mode (lambda () (setq mode-name "λ")))
  (add-hook 'js2-mode-hook (lambda () (setq mode-name "js2"))))

(use-package delight :ensure t)

(use-package eldoc :diminish eldoc-mode)

(use-package time
  :init
  (display-time-mode t)
  (setq display-time-24hr-format t)
  :config
  (setq display-time-day-and-date nil))
