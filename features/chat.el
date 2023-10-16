;; Connection information when using the SourceHut bouncer service. First register the server
;; using the web interface https://chat.sr.ht, then follow the instructions below:
;;
;; - https://man.sr.ht/chat.sr.ht/quickstart.md
;; - https://git.sr.ht/~emersion/soju/tree/master/item/contrib/clients.md
;;
;; `erc-email-userid' must be set as below:
;;   (setq erc-email-userid "<username>/<server>@<machine>")

(require 'erc)

(defvar iensu--erc-nickname nil "ERC nickname")
(defvar iensu--erc-password nil "ERC password")

(setopt erc-auto-query 'bury
        erc-join-buffer 'bury
        erc-interpret-mirc-color t
        erc-rename-buffers t
        erc-lurker-hide-list '("JOIN" "PART" "QUIT")
        erc-track-exclude-types '("JOIN" "NICK" "QUIT" "MODE")
        erc-fill-column 80
        erc-fill-function 'erc-fill-static
        erc-fill-static-center 20)

(use-package erc-hl-nicks :after erc)

(setopt erc-modules '(autoaway
                      autojoin
                      button
                      completion
                      fill
                      irccontrols
                      keep-place
                      list
                      match
                      menu
                      move-to-prompt
                      netsplit
                      networks
                      noncommands
                      readonly
                      ring
                      scrolltobottom
                      stamp
                      track
                      hl-nicks))
(erc-update-modules)

(defun iensu/erc-connect ()
  "Connect to ERC via the SourceHut bouncer service."
  (interactive)
  (message "Connecting to %s with nick %s" erc-email-userid iensu--erc-nickname)
  (erc-tls :server "chat.sr.ht"
           :port 6697
           :nick iensu--erc-nickname
           :password iensu--erc-password))
