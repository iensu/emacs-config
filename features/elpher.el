;; -*- lexical-binding: t; -*-

(use-package elpher
  :ensure t
  :vc (elpher :url "https://github.com/emacsmirror/elpher"
              :rev "d799c467a1f35934f96d33960d638ddf796f01ba"))

(defun iensu-elpher--org-link-store ()
  "Store `elpher' in org-mode."
  (when (eq major-mode 'elpher-mode)
    (let ((link (concat "elpher:" (elpher-info-current)))
          (desc (car elpher-current-page)))
      (message "Handling link %s (%s)" link desc)
      (org-link-store-props :type "elpher"
                            :link link
                            :description desc)
      t)))

(defun iensu-elpher--org-link-follow (link _args)
  "Follow an `elpher' link"
  (require 'elpher)
  (message (concat "Got link: " link))
  (when (or
         (string-match-p "^gemini://.+" link)
         (string-match-p "^gopher://.+" link)
         (string-match-p "^finger://.+" link))
    (elpher-go (string-remove-prefix "elpher:" link))))

(org-link-set-parameters "elpher"
                         :store #'iensu-elpher--org-link-store
                         :follow #'iensu-elpher--org-link-follow)
