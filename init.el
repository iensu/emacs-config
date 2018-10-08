(package-initialize)

(require 'org)
(org-babel-load-file
 (expand-file-name "configuration.org"
                   user-emacs-directory))
