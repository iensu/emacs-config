(require 'typescript-ts-mode)

(add-to-list 'auto-mode-alist '("\\.ts$"  . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx$" . tsx-ts-mode))

(defun iensu--typescript-mode-hook ()
  (lsp-deferred)
  (add-node-modules-path)
  (rainbow-mode 1)
  (prettier-js-mode 1)
  (when (executable-find "eslint")
    (flymake-eslint-enable)))

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
