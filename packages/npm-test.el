;;; -*- lexical-binding: t; -*-

(require 'cl)

(defgroup npm-test nil
  "Tool for running Node scripts."
  :group 'tools)

(defcustom npm-test-default-command "npm test"
  "Default command to be run by `iensu/npm-test'."
  :type '(string)
  :group 'npm-test)

(defcustom npm-test-test-script-regexp "test"
  "Regexp for identifying test-related scripts in package.json"
  :type '(string)
  :group 'npm-test)

(defun npm-test--node-project-root ()
  (locate-dominating-file (or (buffer-file-name) default-directory)
                          "package.json"))

(defun npm-test--test-scripts-list ()
  (let ((default-directory (npm-test--node-project-root)))
    (cl-flet ((find-script-definitions (json-file)
                                       (cdr (assoc 'scripts
                                                   (json-read-file json-file))))

              (get-script-name (script-definition)
                               (symbol-name (car script-definition)))

              (test-script-p (script-name)
                             (and (string-match-p npm-test-test-script-regexp script-name)
                                  (not (string-prefix-p "pre" script-name))
                                  (not (string-prefix-p "post" script-name))))

              (to-npm-script (script-name)
                             (format "npm run %s" script-name)))

      (when (file-exists-p "package.json")
        (let* ((script-defs (find-script-definitions "package.json"))
               (script-names (mapcar #'get-script-name script-defs))
               (test-scripts (remove-if-not #'test-script-p script-names)))
          (mapcar #'to-npm-script test-scripts))))))

;;;###autoload
(defun npm-test-run-tests (&optional command)
  "Run tests for current Node project. Provide COMMAND to run a specific test command."
  (interactive
   (list (if current-prefix-arg
             (completing-read "Command: "
                              (npm-test--test-scripts-list)
                              nil nil)
           nil)))
  (let* ((cmd (or command npm-test-default-command))
         (default-directory (npm-test--node-project-root))
         (project-name (car (last (split-string (iensu/node-project-root) "/")
                                  2)))
         (compilation-buffer-name-function (lambda (_)
                                             (concat "*" cmd ": " project-name "")))
         (compilation-scroll-output t))
    (compile cmd)))

(provide 'npm-test)
