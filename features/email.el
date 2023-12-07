(use-package mu4e
  :ensure nil
  :bind (:map mu4e-view-mode-map
              ("<tab>" . shr-next-link)
              ("<backtab>" . shr-previous-link))
  :hook
  (mu4e-view-mode . visual-line-mode)
  :init
  (require 'mu4e)
  :config
  (setopt mu4e-mu-binary (executable-find "mu"))
  (setopt mail-user-agent 'mu4e-user-agent)
  (setq mu4e-maildir "~/Mail")
  (setopt mu4e-confirm-quit nil)
  (setopt mu4e-context-policy 'pick-first)
  (setopt mu4e-headers-date-format "%Y-%m-%d")
  (setopt mu4e-update-interval nil)
  ;; isync (mbsync) tweaks
  (setopt mu4e-change-filenames-when-moving t) ;; Works better with mbsync
  (setopt mu4e-get-mail-command  (format "INSIDE_EMACS=%s mbsync -a" emacs-version))

  ;; Configuration for viewing emails
  (setq mu4e-view-show-images t)
  (setq mu4e-show-images t)
  (setq mu4e-view-use-gnus t)
  (setq mu4e-view-image-max-width 800)
  (setopt mu4e-compose-format-flowed nil) ;; do not add newlines when sending email
  (setq mu4e-view-show-addresses t)
  (setopt mu4e-headers-fields '((:human-date . 12)
                                (:flags . 6)
                                (:tags . 16)
                                (:from . 22)
                                (:subject)))

  ;; Don't delete, just move to trash
  (defun iensu/mu4e-move-to-trash ()
    "Mark email at point to be moved to trash."
    (interactive)
    (mu4e-mark-set 'move (substring mu4e-trash-folder 0)))

  (define-key mu4e-headers-mode-map (kbd "d") #'iensu/mu4e-move-to-trash)
  (define-key mu4e-view-mode-map    (kbd "d") #'iensu/mu4e-move-to-trash)

  ;; Configuration for composing/sending emails
  (setopt message-send-mail-function 'smtpmail-send-it)
  (setopt smtpmail-debug-info t)
  (setq mu4e-sent-messages-behavior
        (lambda ()
          (if (string-suffix-p "gmail.com" (message-sendmail-envelope-from))
              'delete
            'sent)))
  (setopt message-kill-buffer-on-exit t)
  (setopt mu4e-compose-context-policy 'ask-if-none)

  (add-hook 'mu4e-compose-mode-hook (lambda ()
                                      (auto-fill-mode -1)
                                      (visual-fill-column-mode 1)))

  ;; Add email viewing modes
  (add-to-list 'mu4e-view-actions '("EWW" . iensu--mu4e-view-in-eww) t)
  (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t))

(pretty-hydra-define+ iensu-hydra ()
  ("Email"
   (("e u" iensu/update-email  "update")
    ("e e" mu4e                "open email")
    ("e c" mu4e-compose-new    "write email")
    ("e s" mu4e-headers-search "search email"))))

(defun iensu/mu4e-context (account-name name email smtp-server &optional mail-folder smtp-user vars)
  "Simplify creating MU4E contexts."
  (cl-flet ((is-account-match ()
              (lexical-let ((email email))
                #'(lambda (msg)
                    (when msg
                      (mu4e-message-contact-field-matches msg '(:from :to :cc :bcc) email))))))

    (let* ((folder (or mail-folder
                       (downcase account-name)))
           (smtp-port 465)
           (stream-type 'ssl)
           (vars (append `((mu4e-sent-folder . ,(format "/%s/Sent" folder))
                           (mu4e-drafts-folder . ,(format "/%s/Drafts" folder))
                           (mu4e-trash-folder . ,(format "/%s/Trash" folder))
                           (smtpmail-smtp-user . ,(or smtp-user email))
                           (smtpmail-smtp-service . ,smtp-port)
                           (smtpmail-stream-type . ,stream-type)
                           (smtpmail-smtp-server . ,smtp-server)
                           (user-mail-address . ,email)
                           (user-full-name . ,name))
                         vars)))

      (make-mu4e-context
        :name account-name
        :leave-func (lambda () (setq mu4e-maildir-list nil))
        :match-func (is-account-match)
        :vars vars))))

(defun iensu/fetch-emails ()
  "Fetches emails in the background."
  (interactive)
  (mu4e-update-mail-and-index :run-in-background))
