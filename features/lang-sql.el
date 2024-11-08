(require 'sql)

(add-to-list 'auto-mode-alist '("\\.psql\\'" . sql-mode))

(add-hook 'sql-mode-hook
          (lambda ()
            (when (string= (file-name-extension buffer-file-name) "psql")
              (setopt sql-product 'postgres))))

;; Use `auth-sources' to look for passwords
(setopt sql-password-wallet auth-sources)

;; Make sql-postgres check for passwords
(add-to-list 'sql-postgres-login-params 'password)

(defun sql-comint-postgres (product options &optional buf-name)
  "Create comint buffer and connect to Postgres.

 Overrides the original function in order to be able to use the password
 fetched from `auth-source-search'.

 Originally the arguments are sent as options to `psql', however there is
 no option to pass the password, so here I'm building up a connection URI
 instead."
  (let ((params
         (append options
                 (list (string-join (append
                                     '("postgres://")
                                     (if (not (string= "" sql-user))
                                         (list sql-user))
                                     (if sql-password
                                         (list ":" (if (functionp sql-password) (funcall sql-password) sql-password)))
                                     (if (or sql-password sql-user)
                                         (list "@"))
                                     (if (not (string= "" sql-server))
                                         (list sql-server))
                                     (if (not (= 0 sql-port))
                                         (list ":" (number-to-string sql-port)))
                                     (if (not (string= "" sql-database))
                                         (list "/" sql-database))))))))
    (sql-comint product params buf-name)))
