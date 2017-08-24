;;; init.el --- My configuration file

;;; Commentary:

;;; Code:

(package-initialize)

(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)

(unless (package-installed-p 'pallet)
  (package-install 'pallet))

(require 'pallet)
(pallet-mode t)

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(add-hook 'after-init-hook
          (lambda ()
            (message (format "Emacs started up in %s seconds!" (emacs-init-time)))))

(defun iensu/-config-file (file) (expand-file-name file user-emacs-directory))

(defun iensu/-load-config (&rest files)
  (dolist (fname files)
    (let ((file (iensu/-config-file fname)))
      (if (file-directory-p file)
          (dolist (conf (directory-files-recursively file "\\.el$"))
            (load-file conf))
        (load-file file)))))

(iensu/-load-config
 "user.el"
 "modules/core"
 "modules/org"
 "modules/lang/css.el"
 "modules/lang/elisp.el"
 "modules/lang/javascript.el"
 "modules/lang/json.el"
 "modules/lang/markdown.el"
 "modules/lang/typescript.el"
 "modules/misc"
 )

;;; End packages

(load custom-file 'noerror)

(provide 'init)
;;; init.el ends here
