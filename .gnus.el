;; General settings
(setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\”]\”[#’()]"

      nnml-directory "~/gmail"
      message-directory "~/gmail"

      mm-text-html-renderer 'w3m

      gnus-thread-sort-functions '((not gnus-thread-sort-by-date)
                                   (not gnus-thread-sort-by-number)))

;; Fetching email
(setq gnus-select-method
      '(nnimap "gmail"
               (nnimap-address "imap.gmail.com")
               (nnimap-server-port 993)
               (nnimap-stream ssl)))

;; Sending email
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-local-domain "homepc")

;; Enable topic mode
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(eval-after-load 'gnus-topic
  (lambda ()
    (setq gnus-message-archive-group '((format-time-string "sent.%Y"))
          gnus-topic-topology '(("Gnus" visible)
                                (("misc" visible))
                                (("gmail" visible nil nil)))
          gnus-topic-alist '(("gmail" ; the key of topic
                                   "INBOX"
                                   "[Gmail]/Sent Mail"
                                   "Drafts")
                                  ("misc" ; the key of topic
                                   "nnfolder+archive:sent.2015-12"
                                   "nnfolder+archive:sent.2016"
                                   "nndraft:drafts")
                                  ("Gnus")))))

;; All them hydras

(eval-after-load 'gnus-group
  '(progn
     (defhydra hydra-gnus-group (:color blue)
       "Do?"
       ("a" gnus-group-list-active "REMOTE groups")
       ("l" gnus-group-list-all-groups "LOCAL groups")
       ("c" gnus-topic-catchup-articles "Mark all as read")
       ("G" gnus-group-make-nnir-group "Search server")
       ("g" gnus-group-get-new-news "Refresh")
       ("s" gnus-group-enter-server-mode "Servers")
       ("m" gnus-group-new-mail "Compose")
       ("#" gnus-topic-mark-topic "mark")
       ("q" nil "cancel"))
     ;; y is not used by default
     (define-key gnus-group-mode-map "y" 'hydra-gnus-group/body)))

(eval-after-load 'gnus-sum
  '(progn
     (defhydra hydra-gnus-summary (:color blue)
       "Do?"
       ("n" gnus-summary-insert-new-articles "Refresh")
       ("f" gnus-summary-mail-forward "Forward")
       ("!" gnus-summary-tick-article-forward "Mail -> disk")
       ("p" gnus-summary-put-mark-as-read "Mail <- disk")
       ("c" gnus-summary-catchup-and-exit "Mark all as read")
       ("e" gnus-summary-resend-message-edit "Resend")
       ("R" gnus-summary-reply-with-original "Reply w original")
       ("r" gnus-summary-reply "Reply")
       ("W" gnus-summary-wide-reply-with-original "Reply all w original")
       ("w" gnus-summary-wide-reply "Reply all")
       ("#" gnus-topic-mark-topic "mark")
       ("q" nil "cancel"))
     ;; y is not used by default
     (define-key gnus-summary-mode-map "y" 'hydra-gnus-summary/body)))

(eval-after-load 'gnus-art
  '(progn
     (defhydra hydra-gnus-article (:color blue)
       "Do?"
       ("f" gnus-summary-mail-forward "Forward")
       ("R" gnus-article-reply-with-original "Reply w original")
       ("r" gnus-article-reply "Reply")
       ("W" gnus-article-wide-reply-with-original "Reply all w original")
       ("o" gnus-mime-save-part "Save attachment at point")
       ("w" gnus-article-wide-reply "Reply all")
       ("q" nil "cancel"))
     ;; y is not used by default
     (define-key gnus-article-mode-map "y" 'hydra-gnus-article/body)))

(eval-after-load 'message
  '(progn
     (defhydra hydra-message (:color blue)
       "Do?"
       ("ca" mml-attach-file "Attach")
       ("cc" message-send-and-exit "Send")
       ("q" nil "cancel"))
     (global-set-key (kbd "C-c C-y") 'hydra-message/body)))
