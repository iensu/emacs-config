(defvar iensu-extra-features-alist '()
  "List of enabled extra features which might require additional configurations.")

(defun iensu/tangle-is-feature-enabled (feature)
  "Returns the string `yes' if feature is enabled, `no' otherwise. This is to simplify conditional tangling."
  (if (member feature iensu-extra-features-alist)
      "yes"
    "no"))

;; disable <shift>-<cursor> in org-mode
;; needs to be set before org-mode is loaded
(setq org-replace-disputed-keys t)

;; Load features.el if it exists.
;; features.el should set `iensu-extra-features-alist' to enable features for the
;; specific environment.
(let ((features-file (expand-file-name "features.el"
                                       user-emacs-directory)))
  (when (file-exists-p features-file)
    (load-file features-file)))

;; Always re-tangle configuration.el to make sure that it is up to date.
(let ((elisp-config (expand-file-name "configuration.el" user-emacs-directory)))
  (when (file-exists-p elisp-config)
    (message "Removing configuration.el")
    (delete-file elisp-config)))

(require 'org)
(package-initialize)

;; Tangle configuration.org into configuration.el, while making sure that explicit :tangle yes
;; declarations do not erase already tangled source blocks.
(cl-letf (((symbol-function 'delete-file) #'ignore))
  (org-babel-load-file
   (expand-file-name "configuration.org"
                     user-emacs-directory)))
