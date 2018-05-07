(use-package elfeed
  :ensure t
  :bind (("H-f" . elfeed))
  :config
  (setq elfeed-feeds
        '(("http://planet.emacsen.org/atom.xml" emacs dev)
          ("http://feeds.arstechnica.com/arstechnica/index" tech news)
          ("http://feeds.bbci.co.uk/news/rss.xml" news)
          ("https://hnrss.org/newest" news dev))))
