(package-initialize)

;; disable <shift>-<cursor> in org-mode
;; needs to be set before org-mode is loaded
(setq org-replace-disputed-keys t)

(require 'org)
(org-babel-load-file
 (expand-file-name "configuration.org"
                   user-emacs-directory))
(put 'narrow-to-region 'disabled nil)
