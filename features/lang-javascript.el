;;; Module for JavaScript, TypeScript, Deno and everything else...

(require 'typescript-ts-mode)
(require 'js)
(require 'html-ts-mode)
(require 'css-mode)

(use-package flymake-eslint)
(use-package add-node-modules-path)
(use-package rjsx-mode
  :init
  (add-to-list 'magic-mode-alist
               '((lambda () (and buffer-file-name
                                 (string-equal "js" (file-name-extension buffer-file-name))
                                 (string-match "^import .* from [\"']react[\"']" (buffer-string))))
                 . rjsx-mode))
  :config
  (add-hook 'rjsx-mode-hook (lambda () (setq emmet-expand-jsx-className? t))))
(use-package lsp-biome
  :vc (:url "https://github.com/cxa/lsp-biome" :rev :newest)
  :config
  (defun iensu--biome-hook ()
    (prettier-js-mode -1))
  (add-hook 'lsp-biome-active-hook #'iensu--biome-hook))

(add-to-list 'auto-mode-alist '("\\.ts$"  . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx$" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . rjsx-mode))
(dolist (ext '("\\.js\\'" "\\.mjs\\'" "\\.cjs\\'"))
  (add-to-list 'auto-mode-alist `(,ext . js-ts-mode)))

(defvar iensu--js-embedded-css-font-lock-settings
  (treesit-font-lock-rules
   :language 'css
   :override t
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'css
   :override t
   :feature 'string
   '((string_value) @font-lock-string-face)

   :language 'css
   :override t
   :feature 'keyword
   '(["@media" "@import" "@charset" "@namespace" "@keyframes"] @font-lock-builtin-face
     ["and" "or" "not" "only" "selector"] @font-lock-keyword-face)

   :language 'css
   :override t
   :feature 'variable
   '((plain_value) @font-lock-variable-name-face)

   :language 'css
   :override t
   :feature 'operator
   '(["=" "~=" "^=" "|=" "*=" "$="] @font-lock-operator-face)

   :language 'css
   :override t
   :feature 'selector
   '((class_selector) @css-selector
     (child_selector) @css-selector
     (id_selector) @css-selector
     (tag_name) @css-selector
     (class_name) @css-selector)

   :language 'css
   :override t
   :feature 'property
   '((property_name) @css-property)

   :language 'css
   :override t
   :feature 'function
   '((function_name) @font-lock-function-name-face)

   :language 'css
   :override t
   :feature 'constant
   '((integer_value) @font-lock-number-face
     (float_value) @font-lock-number-face
     (unit) @font-lock-constant-face
     (important) @font-lock-builtin-face)

   :language 'css
   :override t
   :feature 'query
   '((keyword_query) @font-lock-property-use-face
     (feature_name) @font-lock-property-use-face)

   :language 'css
   :override t
   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

   :language 'css
   :override t
   :feature 'error
   '((ERROR) @error))
  "Tree-sitter font-lock settings for css`...` template literals.")

(defun iensu--js-language-at-point (pos)
  "Return the language of the parser covering POS.
Needed as `treesit-language-at-point-function' because creating the HTML and
CSS embedded parsers adds them to the parser list; without this function
`treesit-language-at' returns whichever parser happens to be first in the list
rather than the one whose range covers POS."
  (catch 'lang
    (dolist (parser (treesit-parser-list))
      (when (and (memq (treesit-parser-language parser) '(html css))
                 (treesit-parser-range-on parser pos pos))
        (throw 'lang (treesit-parser-language parser))))
    (treesit-parser-language
     (or (and (boundp 'treesit-primary-parser) treesit-primary-parser)
         (car (treesit-parser-list))))))

;; html-ts-mode--indent-rules has ((parent-is "fragment") column-0 0), which
;; pushes the outermost HTML elements to column 0.  In a template literal the
;; top-level content should keep whatever indentation the author typed, so we
;; replace that rule with no-indent and leave the rest unchanged.
(defvar iensu--js-embedded-html-indent-rules
  `((html
     ((parent-is "fragment") no-indent)
     ((node-is "/>") parent-bol 0)
     ((node-is ">") parent-bol 0)
     ((node-is "end_tag") parent-bol 0)
     ((parent-is "comment") prev-adaptive-prefix 0)
     ((parent-is "element") parent-bol html-ts-mode-indent-offset)
     ((parent-is "script_element") parent-bol html-ts-mode-indent-offset)
     ((parent-is "style_element") parent-bol html-ts-mode-indent-offset)
     ((parent-is "start_tag") parent-bol html-ts-mode-indent-offset)
     ((parent-is "self_closing_tag") parent-bol html-ts-mode-indent-offset)))
  "HTML indent rules for embedded html`...` template literals.")

(defun iensu--js-setup-template-literal-ranges ()
  "Enable HTML/CSS highlighting inside html`...` and css`...` template literals."
  (let ((host-lang (cond ((derived-mode-p 'tsx-ts-mode) 'tsx)
                         ((derived-mode-p 'typescript-ts-mode) 'typescript)
                         (t 'javascript))))
    (when (and (treesit-ready-p host-lang t)
               (treesit-ready-p 'html t)
               (treesit-ready-p 'css t))
      ;; Match template string fragments instead of the whole template_string.
      ;; This keeps embedding stable when `${...}` contains nested html`...`
      ;; expressions, because each static fragment gets an independent range.
      ;; HTML ranges intentionally share one parser (no :local t), which keeps
      ;; context across interpolations so closing tags are highlighted reliably.
      (setq-local treesit-range-settings
                  (append
                   (treesit-range-rules
                    :embed 'html
                    :host host-lang
                    `((call_expression
                       function: (identifier) @_tag
                       (:match "^\\(html\\|HTML\\)$" @_tag)
                       arguments: (template_string
                                   (string_fragment) @capture))))
                   (treesit-range-rules
                    :embed 'css
                    :host host-lang
                    `((call_expression
                       function: (identifier) @_tag
                       (:match "^\\(css\\|CSS\\)$" @_tag)
                       arguments: (template_string
                                   (string_fragment) @capture))))
                   (or treesit-range-settings '())))
      (treesit-update-ranges)
      ;; With multiple parsers in the buffer (JS + HTML + CSS), treesit-language-at
      ;; returns the first parser's language for every position, which is wrong.
      ;; This function checks the embedded parser ranges instead.
      (setq-local treesit-language-at-point-function
                  #'iensu--js-language-at-point)
      ;; Add indent rules for embedded languages.  Use modified HTML rules that
      ;; do not force top-level elements to column-0 (see iensu--js-embedded-html-indent-rules).
      (setq-local treesit-simple-indent-rules
                  (append treesit-simple-indent-rules
                          iensu--js-embedded-html-indent-rules
                          css--treesit-indent-rules))
      ;; treesit-add-font-lock-rules explicitly enables all rules (enable=t).
      (treesit-add-font-lock-rules html-ts-mode--font-lock-settings)
      (treesit-add-font-lock-rules iensu--js-embedded-css-font-lock-settings)
      (font-lock-flush))))

(defun iensu--deno-project-p ()
  (let ((fname (buffer-file-name)))
    (or (locate-dominating-file fname "deno.json")
        (locate-dominating-file fname ".deno-project"))))

(defun iensu--typescript-hook ()
  ;; Ensure we use deno lsp for deno projects
  (let ((js-clients '(ts-ls jsts-ls)))
    (if (and (iensu--deno-project-p)
             (executable-find "deno"))
        (dolist (client js-clients)
          (add-to-list (make-local-variable 'lsp-disabled-clients) client))
      (set (make-local-variable 'lsp-disabled-clients) (cl-remove-if
                                                        (lambda (c) (seq-contains-p js-clients
                                                                                    c))
                                                        lsp-disabled-clients))))
  (lsp-deferred)
  (add-node-modules-path)
  (rainbow-mode 1)
  (when (executable-find "prettier")
    (prettier-js-mode 1))
  (flymake-mode 1)
  (setq-local forward-sexp-function #'forward-sexp-default-function)
  (when (executable-find "eslint")
    (flymake-eslint-enable))
  (iensu--js-setup-template-literal-ranges))

(defun iensu--typescript-jsx-hook ()
  (iensu--typescript-hook)
  (emmet-mode 1))

(add-hook 'typescript-ts-mode-hook #'iensu--typescript-hook)
(add-hook 'js-ts-mode-hook         #'iensu--typescript-hook)
(add-hook 'tsx-ts-mode-hook        #'iensu--typescript-jsx-hook)
(add-hook 'rjsx-mode-hook          #'iensu--typescript-jsx-hook)

(defun iensu/typescript-compile ()
  (interactive)
  (project-compile "tsc --pretty false"))

(define-key typescript-ts-mode-map (kbd "C-c C-c") #'iensu/typescript-compile)
(define-key tsx-ts-mode-map        (kbd "C-c C-c") #'iensu/typescript-compile)
