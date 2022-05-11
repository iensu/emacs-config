(use-package mu4e
  :straight nil
  :bind (:map mu4e-view-mode-map
              ("<tab>" . shr-next-link)
              ("<backtab>" . shr-previous-link))
  :hook
  (mu4e-view-mode . visual-line-mode)
  :init
  (require 'mu4e)
  :config
  (setq mu4e-mu-binary (executable-find "mu"))
  (setq mail-user-agent 'mu4e-user-agent)
  (setq mu4e-maildir iensu-email-directory)
  (setq mu4e-confirm-quit nil)
  (setq mu4e-context-policy 'pick-first)
  (setq mu4e-headers-date-format "%Y-%m-%d")

  ;; Configuration for viewing emails
  (setq mu4e-view-show-images t)
  (setq mu4e-show-images t)
  (setq mu4e-view-use-gnus t)
  (setq mu4e-view-image-max-width 800)
  (setq mu4e-compose-format-flowed t)
  (setq mu4e-view-show-addresses t)
  (setq mu4e-headers-fields '((:human-date . 12)
                              (:flags . 6)
                              (:tags . 16)
                              (:from . 22)
                              (:subject)))

  ;; Configuration for composing/sending emails
  (setq user-full-name "Jens Östlund")
  (setq user-mail-address "jostlund@gmail.com")
  (setq message-send-mail-function 'smtpmail-send-it)
  (setq smtpmail-debug-info t)
  (setq mu4e-sent-messages-behavior 'delete)
  (setq message-kill-buffer-on-exit t)
  (setq mu4e-compose-context-policy 'ask-if-none)

  (add-hook 'mu4e-compose-mode-hook (lambda () (auto-fill-mode -1)))

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

(defun iensu/mu4e-context (account-name name email smtp-server &optional vars)
  "Simplify creating MU4E contexts."
  (cl-flet ((is-account-match ()
              (lexical-let ((email email))
                #'(lambda (msg)
                    (when msg
                      (mu4e-message-contact-field-matches msg '(:from :to :cc :bcc) email))))))

    (let* ((folder (concat "/" (downcase account-name)))
           (vars (append `((mu4e-sent-folder . ,(format "%s/Sent" folder))
                           (mu4e-drafts-folder . ,(format "%s/Drafts" folder))
                           (mu4e-trash-folder . ,(format "%s/Trash" folder))
                           (smtpmail-smtp-user . ,email)
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
