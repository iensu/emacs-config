;;; modules/misc/email.el --- Email -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defvar iensu-mail-dir "~/Mail")

(let ((mu4e-path "/usr/local/share/emacs/site-lisp/mu/mu4e"))
  (when (and (executable-find "mu")
             (file-directory-p mu4e-path))
    (add-to-list 'load-path (concat mu4e-path "/mu4e.elc"))
    (require 'mu4e)
    (eval-after-load "mu4e"
      (progn
        (setq mu4e-maildir iensu-mail-dir
              mu4e-context-policy 'pick-first
              mu4e-confirm-quit nil
              mu4e-mu-binary "/usr/local/bin/mu"
              mu4e-get-mail-command "offlineimap"
              mu4e-user-mail-address-list '("jens.ostlund@futurice.com" "jostlund@gmail.com")
              mu4e-sent-messages-behavior 'delete
              mu4e-update-interval 180 ;; seconds
              mu4e-contexts
              `(,(make-mu4e-context
                  :name "private"
                  :enter-func (lambda () (mu4e-message "Entering private context"))
                  :leave-func (lambda () (mu4e-message "Leaving private context"))
                  :match-func
                  (lambda (msg)
                    (when msg
                      (mu4e-message-contact-field-matches msg :to "jostlund@gmail.com")))
                  :vars '((user-mail-address            . "jostlund@gmail.com")
                          (user-full-name               . "Jens Östlund")
                          (mu4e-sent-folder             . "/private/sent")
                          (mu4e-drafts-folder           . "/private/drafts")
                          (smtpmail-smtp-user           . "jostlund@gmail.com")
                          (smtpmail-local-domain        . "gmail.com")
                          (smtpmail-default-smtp-server . "smtp.gmail.com")
                          (smtpmail-smtp-server         . "smtp.gmail.com")
                          (smtpmail-smtp-service        . 587)))
                ,(make-mu4e-context
                  :name "futurice"
                  :match-func
                  (lambda (msg)
                    (when msg
                      (mu4e-message-contact-field-matches msg :to "jens.ostlund@futurice.com")))
                  :vars '((user-mail-address            . "jens.ostlund@futurice.com")
                          (user-full-name               . "Jens Östlund")
                          (mu4e-sent-folder             . "/futurice/sent")
                          (mu4e-drafts-folder           . "/futurice/drafts")
                          (smtpmail-smtp-user           . "jens.ostlund@futurice.com")
                          (smtpmail-local-domain        . "futurice.com")
                          (smtpmail-default-smtp-server . "smtp.gmail.com")
                          (smtpmail-smtp-server         . "smtp.gmail.com")
                          (smtpmail-smtp-service        . 587))))

              message-send-mail-function 'smtpmail-send-it
              smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
              smtpmail-auth-credentials '(("smtp.gmail.com" 587 "jens.ostlund@futurice.com" nil))
              smtpmail-default-smtp-server "smtp.gmail.com"
              smtpmail-smtp-server "smtp.gmail.com"
              smtpmail-smtp-service 587
              message-kill-buffer-on-exit t
              mail-user-agent 'mu4e-user-agent)
        (iensu-add-to-list 'mu4e-bookmarks
                           `(,(make-mu4e-bookmark
                               :name "Futurice inbox"
                               :query "to:jens.ostlund@futurice.com tag:\"\\\\Inbox\""
                               :key ?f)
                             ,(make-mu4e-bookmark
                               :name "Private inbox"
                               :query "to:jostlund@gmail.com tag:\"\\\\Inbox\""
                               :key ?p)))))))

(use-package mu4e-alert
  :after mu4e
  :init
  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display))
