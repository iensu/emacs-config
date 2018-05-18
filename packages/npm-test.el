;;; npm-test.el --- Package for running NPM test scripts -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Jens Östlund

;; Author: Jens Östlund <jostlund@gmail.com>
;; Keywords: tools
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a command `npm-test-run-tests' for running NPM tests for a
;; node.js project in a compilation buffer.

;;; Code:

(require 'cl-lib)
(require 'json)

(defgroup npm-test nil
  "Tool for running Node scripts."
  :group 'tools)

(defcustom npm-test-default-command "npm test"
  "Default command to be run by `npm-test-run-tests'."
  :type '(string)
  :group 'npm-test)

(defcustom npm-test-test-script-regexp "test"
  "Regexp for identifying test-related scripts in package.json."
  :type '(string)
  :group 'npm-test)

(defvar npm-test--previously-selected-script nil
  "Stores the previously selected script command.")

(defun npm-test--node-project-root ()
  "Return the project root directory for a node.js project."
  (locate-dominating-file (or (buffer-file-name) default-directory)
                          "package.json"))

(defun npm-test--test-scripts-list ()
  "Return a list of test script names based on the contents of package.json.

What designates a test script is controlled by `npm-test-test-script-regexp',
pre- and post scripts are however always excluded from the list."
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
               (test-scripts (cl-remove-if-not #'test-script-p script-names)))
          (mapcar #'to-npm-script test-scripts))))))

;;;###autoload
(defun npm-test-run-tests (&optional command)
  "Run test script for current Node project.

The default command is defined by the customizable variable
`npm-test-default-command'.

Provide an optional COMMAND to run a specific test command. If run
interactively with an argument (C-u M-x `npm-test-run-tests') a list
of available test script candidates will appear in the minibuffer.
For a script to be considered a test script it needs to match the regexp
in the customizable variable `npm-test-test-script-regexp'."
  (interactive
   (list (if current-prefix-arg
             (completing-read (if npm-test--previously-selected-script
                                  (format "Command (%s): " npm-test--previously-selected-script)
                                "Command: ")
                              (npm-test--test-scripts-list)
                              nil nil nil nil npm-test--previously-selected-script)
           nil)))
  (when command
    (setq npm-test--previously-selected-script command))
  (let* ((cmd (or command npm-test-default-command))
         (default-directory (npm-test--node-project-root))
         (project-name (car (last (split-string (npm-test--node-project-root) "/")
                                  2)))
         (compilation-buffer-name-function (lambda (_)
                                             (concat "*" cmd ": " project-name "")))
         (compilation-scroll-output t))
    (compile cmd)))

(provide 'npm-test)
;;; npm-test.el ends here
