(use-package elpher)

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
