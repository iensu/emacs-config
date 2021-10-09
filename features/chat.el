(defcustom iensu-chat-config nil "A plist of options to connect to an IRC chat."
  :group 'iensu-chat)

(use-package erc
  :commands (erc erc-tls)
  :config
  (setq erc-prompt-for-nickserv-password t
        erc-auto-query 'bury
        erc-join-buffer 'bury
        erc-interpret-mirc-color t
        erc-rename-buffers t
        erc-lurker-hide-list '("JOIN" "PART" "QUIT")
        erc-track-exclude-types '("JOIN" "NICK" "QUIT" "MODE")
        erc-fill-column 80
        erc-fill-function 'erc-fill-static
        erc-fill-static-center 20
        erc-autojoin-channels-alist '(("libera.chat" . ("#emacs" "#systemcrafters" "#anime")))
        erc-modules '(autoaway
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
                      stamp
                      track
                      hl-nicks)))

(use-package erc-hl-nicks :after erc)

(use-package erc-image
  :straight (erc-image :type git :host github :repo "kidd/erc-image.el")
  :config
  (add-to-list 'erc-modules 'image)
  (erc-update-modules)
  (setq-default erc-image-inline-rescale 'window))

(defun iensu/erc-connect ()
  "Connect to ERC with the configuration specified by `iensu-chat-config'."
  (interactive)
  (erc-tls :server (plist-get iensu-chat-config :server)
           :port (plist-get iensu-chat-config :port)
           :nick (plist-get iensu-chat-config :nick)))
