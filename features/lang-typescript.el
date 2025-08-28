(require 'typescript-ts-mode)

(add-to-list 'auto-mode-alist '("\\.ts$"  . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx$" . tsx-ts-mode))

(require 'eglot)


(defun iensu--deno-ts-project-p ()
  (when-let* ((project (project-current))
              (p-root (project-root project)))
    (or (file-exists-p (concat p-root "deno.json"))
        (file-exists-p (concat p-root ".deno-project")))))

(defun iensu--ts-server-program (&rest _)
  (cond ((iensu--deno-ts-project-p) '("deno" "lsp" :initializationOptions
                                         (:enable t :lint t)))
         (t '("typescript-language-server" "--stdio"))))

(add-to-list 'eglot-server-programs '((typescript-ts-mode :language-id "typescript") . iensu--ts-server-program))

(defun iensu--typescript-mode-hook ()
  (eglot-ensure)
  (eglot-inlay-hints-mode 1)
  (add-node-modules-path)
  (rainbow-mode 1)
  (prettier-js-mode 1)
  (flymake-mode 1)
  (setq-local forward-sexp-function #'forward-sexp-default-function)
  (when (executable-find "eslint")
    (flymake-eslint-enable)))

(use-package flymake-eslint)

(add-hook 'typescript-ts-mode-hook #'iensu--typescript-mode-hook)
(add-hook 'tsx-ts-mode-hook #'iensu--typescript-mode-hook)

(defun iensu/typescript-compile ()
  (interactive)
  (compile "tsc --pretty false"))

(defun iensu/nextjs-compile ()
  (interactive)
  (let ((default-directory (expand-file-name (locate-dominating-file "." ".git"))))
    (compile "next build")))

(defun iensu/nextjs-lint ()
  (interactive)
  (let ((default-directory (expand-file-name (locate-dominating-file "." ".git"))))
    (compile "next lint")))

(define-key typescript-ts-mode-map (kbd "C-c C-c") #'iensu/typescript-compile)
(define-key tsx-ts-mode-map (kbd "C-c C-c") #'iensu/typescript-compile)

(defun iensu/tsx-ts-mark-node ()
  (interactive)
  (let ((node (treesit-node-parent (treesit-node-at (point)))))
    (push-mark (treesit-node-end node) nil t)))

(defalias 'mn 'iensu/tsx-ts-mark-node)
