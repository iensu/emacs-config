;;; grafana-alloy-mode.el --- major mode for Grafana Alloy files  -*- lexical-binding: t; -*-

;;; Commentary:

;; A lightweight major mode for Grafana Alloy's HCL-inspired configuration
;; language.  Alloy configuration files contain attributes, expressions, and
;; nested blocks, for example:
;;
;;   prometheus.scrape "default" {
;;     targets    = discovery.kubernetes.pods.targets
;;     forward_to = [prometheus.remote_write.default.receiver]
;;   }

;;; Code:

(require 'cc-mode)

(defconst grafana-alloy--identifier-regexp
  "[[:alpha:]_][[:alnum:]_.-]*"
  "Regexp matching an Alloy identifier or dotted component name.")

(defconst grafana-alloy--literal-regexp
  (regexp-opt '("true" "false" "null") 'symbols)
  "Regexp matching Alloy literal values.")

(defvar grafana-alloy-mode-syntax-table
  (let ((table (copy-syntax-table c-mode-syntax-table)))
    ;; C's syntax table supplies the same // and /* ... */ comment behavior
    ;; needed by Alloy, while the remaining entries are adjusted below.
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?{ "(}" table)
    (modify-syntax-entry ?} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    table)
  "Syntax table for `grafana-alloy-mode'.")

(defconst grafana-alloy-font-lock-keywords
  `(
    ;; A block name is followed by zero or more labels and an opening brace.
    (,(concat "\\_<\\(" grafana-alloy--identifier-regexp
              "\\)\\>[[:space:]]*"
              "\\(?:\"[^\"\\n]*\"[[:space:]]*\\)*{")
      1 font-lock-function-name-face)
    ;; Attribute names occur immediately before an equals sign.
    (,(concat "\\_<\\(" grafana-alloy--identifier-regexp
              "\\)\\>[[:space:]]*=")
      1 font-lock-variable-name-face)
    ;; Function calls and component references are useful navigation cues.
    (,(concat "\\_<\\(" grafana-alloy--identifier-regexp
              "\\)\\>[[:space:]]*(")
      1 font-lock-function-name-face)
    (,(concat "\\_<\\(" grafana-alloy--identifier-regexp
              "\\.[[:alpha:]_][[:alnum:]_.-]*\\)\\>")
      1 font-lock-variable-name-face)
    (,grafana-alloy--literal-regexp . font-lock-constant-face)
    ("\\_<[[:digit:]]+\\(?:\\.[[:digit:]]+\\)?\\_>" . font-lock-constant-face)
    ("\\(?:=>\\|==\\|!=\\|<=\\|>=\\|&&\\|||\\|[=+*/<>?-]\\)"
     . font-lock-keyword-face))
  "Font-lock keywords for `grafana-alloy-mode'.")

(defun grafana-alloy--indent-line ()
  "Indent the current Alloy line according to surrounding delimiters."
  ;; Font-lock can have changed syntax properties immediately before an
  ;; indentation command.  Refresh the cache so delimiter depth is current.
  (syntax-ppss-flush-cache (line-beginning-position))
  (let ((indent (* (car (syntax-ppss)) tab-width)))
    (save-excursion
      (back-to-indentation)
      (when (looking-at "[])}]")
        (setq indent (max 0 (- indent tab-width)))))
    (indent-line-to indent)))

;;;###autoload
(define-derived-mode grafana-alloy-mode prog-mode "Grafana-Alloy"
  "Major mode for editing Grafana Alloy configuration files."
  :syntax-table grafana-alloy-mode-syntax-table
  (setq-local font-lock-defaults '(grafana-alloy-font-lock-keywords))
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local indent-line-function #'grafana-alloy--indent-line)
  (electric-pair-mode 1))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.alloy\\'" . grafana-alloy-mode))

(provide 'grafana-alloy-mode)

;;; grafana-alloy-mode.el ends here
