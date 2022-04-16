(use-package w3m)

(defun iensu/query-ddg (query)
  "Send a query to DuckDuckGo using w3m"
  (interactive "sQuery (DDG): ")
  (let ((q (shr-encode-url query)))
    (w3m-goto-url (format "https://duckduckgo.com?q=%s" q))))

(pretty-hydra-define+ iensu-hydra ()
  ("Browsing"
   (("w w" w3m-goto-url       "go to URL")
    ("w q" iensu/query-ddg    "query DuckDuckGo")
    ("w b" w3m-bookmark-view "w3m bookmarks"))))
