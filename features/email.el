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
  (setq mu4e-maildir iensu-email-directory)
  (setopt mu4e-confirm-quit nil)
  (setopt mu4e-context-policy 'pick-first)
  (setopt mu4e-headers-date-format "%Y-%m-%d")

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

  ;; Configuration for composing/sending emails
  (setopt user-full-name "Jens Östlund")
  (setopt user-mail-address "jostlund@gmail.com")
  (setopt message-send-mail-function 'smtpmail-send-it)
  (setopt smtpmail-debug-info t)
  (setopt mu4e-sent-messages-behavior 'delete)
  (setopt message-kill-buffer-on-exit t)
  (setopt mu4e-compose-context-policy 'ask-if-none)

  (add-hook 'mu4e-compose-mode-hook (lambda ()
                                      (auto-fill-mode -1)
                                      (visual-fill-column-mode 1)))

  ;; Add email viewing modes
  (add-to-list 'mu4e-view-actions '("EWW" . iensu--mu4e-view-in-eww) t)
  (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t))

(defun iensu/update-email ()
  "Fetches email ensuring that we are \"authenticated\" and updates the mu index."
  (interactive)
  (when (executable-find "offlineimap")
    (epa-decrypt-file "~/.dummy.yaml.gpg" "/dev/null")
    (let* ((command "offlineimap -o")
           (process (start-process-shell-command "offlineimap" "*offlineimap -o*" command)))
      (set-process-sentinel process
                            (lambda (proc event)
                              (cond ((string-match-p "finished" event) (progn
                                                                         (message "Fetched email, updating index...")
                                                                         (kill-buffer "*offlineimap -o*")
                                                                         (mu4e-update-index)))))))))

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
                       (concat "/" (downcase account-name))))
           (vars (append `((mu4e-sent-folder . ,(format "/%s/Sent" folder))
                           (mu4e-drafts-folder . ,(format "/%s/Drafts" folder))
                           (mu4e-trash-folder . ,(format "/%s/Trash" folder))
                           (smtpmail-smtp-user . ,(or smtp-user email))
                           (smtpmail-smtp-service . 465)
                           (smtpmail-stream-type . ssl)
                           (smtpmail-smtp-server . ,smtp-server)
                           (user-mail-address . ,email)
                           (user-full-name . ,name))
                         vars)))

      (make-mu4e-context
        :name account-name
        :leave-func (lambda () (setq mu4e-maildir-list nil))
        :match-func (is-account-match)
        :vars vars))))
