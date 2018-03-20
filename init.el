;;; init.el --- My configuration file

;;; Commentary:

;;; Code:

(package-initialize)

(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)

(setq gc-cons-threshold 100000000)

(unless (package-installed-p 'pallet)
  (package-install 'pallet))

(require 'pallet)
(pallet-mode t)

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-defer nil
	use-package-always-ensure nil))

(require 'bind-key)

(add-hook 'after-init-hook
          (lambda ()
            (message (format "Emacs started up in %s seconds!" (emacs-init-time)))))

;; define here so all packages can access
(define-prefix-command 'iensu-map)

(defun iensu--config-file (file) (expand-file-name file user-emacs-directory))

(defun iensu--load-config (&rest files)
  (dolist (fname files)
    (let ((file (iensu--config-file fname)))
      (if (file-directory-p file)
          (dolist (conf (directory-files-recursively file "\\.el$"))
            (load-file conf))
        (load-file file)))))

(iensu--load-config
 "user.el"
 "modules/core"
 "modules/org"
 "modules/lang/css.el"
 "modules/lang/elisp.el"
;; "modules/lang/elixir.el"
;; "modules/lang/elm.el"
 "modules/lang/javascript.el"
 "modules/lang/json.el"
 "modules/lang/markdown.el"
 "modules/lang/rust.el"
 "modules/lang/typescript.el"
 "modules/misc"
 )

(load-file "~/Projects/private/org-github-issues/org-github-issues.el")

;;; End packages

(load custom-file 'noerror)

(provide 'init)
;;; init.el ends here
