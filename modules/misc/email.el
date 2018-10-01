;;; modules/misc/email.el --- Email -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defun iensu--render-html-message ()
  (let ((dom (libxml-parse-html-region (point-min) (point-max))))
    (erase-buffer)
    (shr-insert-document dom)
    (goto-char (point-min))))


(defun iensu--mu4e-setup ()
  (setq mail-user-agent 'mu4e-user-agent
        mu4e-mu-binary "/usr/local/bin/mu"

        mu4e-maildir "~/Mail"
        mu4e-maildir-shortcuts
        '(("/futurice/All mail" . ?F)
          ("/private/All mail" . ?P))

        mu4e-sent-messages-behavior 'delete
        mu4e-update-interval 180

        mu4e-context-policy 'pick-first
        mu4e-confirm-quit nil
        message-kill-buffer-on-exit t

        mu4e-get-mail-command "offlineimap"

        mu4e-view-show-images t
        mu4e-show-images t
        mu4e-view-image-max-width 800

        mu4e-compose-format-flowed t
        mu4e-view-show-addresses t

        mu4e-headers-fields '((:human-date . 12)
                              (:flags . 6)
                              (:tags . 16)
                              (:from . 22)
                              (:subject))

        mu4e-compose-context-policy 'ask-if-none
        mu4e-contexts
        `(,(make-mu4e-context
            :name "Futurice"
            :enter-func (lambda () (mu4e-message "Entering Futurice context"))
            :leave-func (lambda () (setq mu4e-maildir-list nil)) ; forces refresh of address list when switching context
            :match-func (lambda (msg)
                          (when msg
                            (string-match-p "^/futurice" (mu4e-message-field msg :maildir))))
            :vars '((mu4e-sent-folder   . "/futurice/sent")
                    (mu4e-drafts-folder . "/futurice/drafts")
                    (mu4e-trash-folder  . "/futurice/trash")

                    (user-mail-address  . "jens.ostlund@futurice.com")
                    (user-full-name     . "Jens Östlund")

                    (smtpmail-smtp-user . "jens.ostlund@futurice.com")))

          ,(make-mu4e-context
            :name "Private"
            :enter-func (lambda () (mu4e-message "Entering Private context"))
            :leave-func (lambda () (setq mu4e-maildir-list nil)) ; forces refresh of address list when switching context
            :match-func (lambda (msg)
                          (when msg
                            (string-match-p "^/private" (mu4e-message-field msg :maildir))))
            :vars '((mu4e-sent-folder   . "/private/sent")
                    (mu4e-drafts-folder . "/private/drafts")
                    (mu4e-trash-folder  . "/private/trash")

                    (user-mail-address  . "jostlund@gmail.com")
                    (user-full-name     . "Jens Östlund")

                    (smtpmail-smtp-user . "jostlund")))))

  (add-to-list 'mu4e-view-actions '("EWW" . iensu--mu4e-view-in-eww) t)
  (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)

  ;; message viewing settings
  (add-hook 'mu4e-view-mode-hook
            (lambda ()
              (local-set-key (kbd "<tab>") 'shr-next-link)
              (local-set-key (kbd "<backtab>") 'shr-previous-link)))
  (setq shr-color-visible-luminance-min 80))

(defun iensu--send-email-setup ()
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 465
        smtpmail-stream-type 'ssl
        smtpmail-debug-info t))

(add-hook 'message-mode-hook 'turn-on-orgtbl)
(add-hook 'message-mode-hook 'turn-on-orgstruct++)

(setq user-mail-address "jens.ostlund@futurice.com"
      user-full-name "Jens Östlund")

(let ((mu4e-path "/usr/local/share/emacs/site-lisp/mu/mu4e"))
  (when (and (executable-find "mu")
             (file-directory-p mu4e-path))
    (add-to-list 'load-path mu4e-path)
    (require 'mu4e)
    (eval-after-load "mu4e"
      (progn
        (iensu--mu4e-setup)
        (iensu--send-email-setup)))))

(use-package mu4e-alert
  :after mu4e
  :init
  (mu4e-alert-set-default-style 'notifier)
  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
  (add-hook 'after-init-hook #'mu4e-alert-enable-notifications))

(use-package org-mu4e)
