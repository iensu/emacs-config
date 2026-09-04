(use-package elfeed :ensure t)

(transient-append-suffix 'iensu-transient (list 3)
  ["Elfeed"
   ("f f" "open" elfeed)
   ("f u" "update" elfeed-update)])
