(use-package w3m :ensure t)

(defun iensu/query-ddg (query)
  "Send a query to DuckDuckGo using w3m"
  (interactive "sQuery (DDG): ")
  (let ((q (shr-encode-url query)))
    (w3m-goto-url (format "https://duckduckgo.com?q=%s" q))))

(transient-append-suffix 'iensu-transient (list 3)
  ["Browsing"
   ("w w" "go to URL" w3m-goto-url)
   ("w q" "query DuckDuckGo" iensu/query-ddg)
   ("w b" "w3m bookmarks" w3m-bookmark-view)])
