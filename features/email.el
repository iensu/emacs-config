(use-package mu4e
  :ensure nil
  :load-path iensu-email-mu4e-package-path
  :bind (:map mu4e-view-mode-map
              ("<tab>" . shr-next-link)
              ("<backtab>" . shr-previous-link))
  :hook
  (mu4e-view-mode . visual-line-mode)
  :init
  (require 'mu4e)
  :config
  (setq mail-user-agent 'mu4e-user-agent)
  (setq mu4e-mu-binary iensu-email-mu-binary-path)
  (setq mu4e-maildir iensu-email-directory)
  (setq mu4e-confirm-quit nil)
  (setq mu4e-context-policy 'pick-first)

  ;; Configuration for viewing emails
  (setq mu4e-view-show-images t)
  (setq mu4e-show-images t)
  (setq mu4e-view-image-max-width 800)
  (setq mu4e-compose-format-flowed t)
  (setq mu4e-view-show-addresses t)
  (setq mu4e-headers-fields '((:human-date . 12)
                              (:flags . 6)
                              (:tags . 16)
                              (:from . 22)
                              (:subject)))

  ;; Configuration for composing/sending emails
  (setq user-mail-address "jens.ostlund@futurice.com")
  (setq user-full-name "Jens Östlund")
  (setq message-send-mail-function 'smtpmail-send-it)
  (setq smtpmail-debug-info t)
  (setq mu4e-sent-messages-behavior 'delete)
  (setq message-kill-buffer-on-exit t)
  (setq mu4e-compose-context-policy 'ask-if-none)

  (add-hook 'mu4e-compose-mode-hook (lambda () (auto-fill-mode -1)))

  ;; Add email viewing modes
  (add-to-list 'mu4e-view-actions '("EWW" . iensu--mu4e-view-in-eww) t)
  (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t))

(pretty-hydra-define+ iensu-hydra ()
  ("Email"
   (("e u" mu4u-update-index               "update" :exit nil)
    ("e e" mu4e                            "open email")
    ("e c" mu4e-compose-new                "write email")
    ("e s" mu4e-headers-search             "search email"))))
