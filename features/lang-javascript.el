;;; Module for JavaScript, TypeScript, Deno and everything else...

(require 'typescript-ts-mode)
(require 'js)

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

(add-to-list 'auto-mode-alist '("\\.ts$"  . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx$" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . rjsx-mode))
(dolist (ext '("\\.js\\'" "\\.mjs\\'" "\\.cjs\\'"))
  (add-to-list 'auto-mode-alist `(,ext . js-ts-mode)))

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
  (prettier-js-mode 1)
  (flymake-mode 1)
  (setq-local forward-sexp-function #'forward-sexp-default-function)
  (when (executable-find "eslint")
    (flymake-eslint-enable)))

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
