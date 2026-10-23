;;; helmfile-gotmpl-mode.el --- major mode for helmfile Go-template (.gotmpl) files  -*- lexical-binding: t; -*-

;;; Commentary:

;; A `yaml-ts-mode' derivative that layers Go-template ({{ ... }}) syntax
;; highlighting on top of the inherited YAML tree-sitter fontification, for
;; helmfile config/values files such as `helmfile.yaml.gotmpl'.
;;
;; Put this file into your load-path and:
;;   (require 'helmfile-gotmpl-mode)

;;; Code:

(require 'yaml-ts-mode)

(defconst helmfile-gotmpl--keywords-regexp
  (regexp-opt '("if" "else" "end" "range" "with" "define" "template"
                "block" "break" "continue" "and" "or" "not"
                "eq" "ne" "lt" "le" "gt" "ge" "true" "false" "nil")
              'symbols)
  "Regexp matching Go-template control-flow/operator keywords.")

(defconst helmfile-gotmpl--action-token-regexp
  (concat "\\(?1:\\$[[:alpha:]_][[:alnum:]_]*\\)"        ; $variable
          "\\|\\(?2:\\.[[:alpha:]_][[:alnum:]_.]*\\)"     ; .Field.Access
          "\\|\\(?3:" helmfile-gotmpl--keywords-regexp "\\)" ; keyword
          "\\|\\(?4:\"[^\"]*\"\\)"                         ; "string"
          "\\|\\(?5:[[:alpha:]_][[:alnum:]_]*\\)")         ; bare identifier (function)
  "Regexp matching the token kinds found inside a Go-template action.")

(defun helmfile-gotmpl--action-bound ()
  "PRE-MATCH-FORM for the action-token anchored matcher.
Called right after matching the opening `{{'/`{{-' delimiter.  Returns
the position of the action's closing delimiter, so the anchored search
for inner tokens is confined to a single action.  For `{{/* ... */}}'
comments (handled by a separate, simpler rule) it returns the current
point, i.e. a zero-width bound, so no token search happens there."
  (if (looking-at "[ \t]*/\\*")
      (point)
    (save-excursion
      (if (re-search-forward "-?}}" nil t)
          (match-beginning 0)
        (point-max)))))

(defvar helmfile-gotmpl-font-lock-keywords
  `(
    ;; single-line {{/* ... */}} comments
    ("{{-?[ \t]*/\\*.*?\\*/[ \t]*-?}}" . font-lock-comment-face)
    ;; closing delimiter
    ("-?}}" . font-lock-preprocessor-face)
    ;; opening delimiter, with its action's contents fontified via an
    ;; anchored sub-search bounded by `helmfile-gotmpl--action-bound'
    ("{{-?"
     (0 font-lock-preprocessor-face)
     (,helmfile-gotmpl--action-token-regexp
      (helmfile-gotmpl--action-bound)
      nil
      (1 font-lock-variable-name-face nil t)
      (2 font-lock-variable-name-face nil t)
      (3 font-lock-keyword-face nil t)
      (4 font-lock-string-face nil t)
      (5 font-lock-function-name-face nil t))))
  "Font-lock keywords for Go-template syntax inside `helmfile-gotmpl-mode'.")

;;;###autoload
(define-derived-mode helmfile-gotmpl-mode yaml-ts-mode "Helmfile[gotmpl]"
  "Major mode for helmfile Go-template (.gotmpl) files.
Derived from `yaml-ts-mode', with Go-template `{{ ... }}' actions
additionally fontified."
  (font-lock-add-keywords nil helmfile-gotmpl-font-lock-keywords 'append))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.gotmpl\\'" . helmfile-gotmpl-mode))

(provide 'helmfile-gotmpl-mode)

;;; helmfile-gotmpl-mode.el ends here
