(require 'js)

(dolist (ext '("\\.js\\'" "\\.mjs\\'" "\\.cjs\\'"))
  (add-to-list 'auto-mode-alist `(,ext . js-mode)))

(define-key js-ts-mode-map (kbd "C-c C-c") #'compile)

(defun iensu/javascript-mode-hook ()
  (electric-indent-mode 1)
  (smartparens-mode 1)
  (eglot-ensure)
  (prettier-js-mode 1)
  (js-ts-mode 1))

(add-hook 'js-mode-hook #'iensu/javascript-mode-hook)

(use-package rjsx-mode
  :mode ("\\.jsx\\'")
  :hook
  (rjsx-mode . electric-indent-mode)
  (rjsx-mode . rainbow-delimiters-mode)
  (rjsx-mode . emmet-mode)
  (rjsx-mode . eglot-ensure)
  (rjsx-mode . prettier-js-mode)
  :init
  (add-to-list 'magic-mode-alist
               '((lambda () (and buffer-file-name
                            (string-equal "js" (file-name-extension buffer-file-name))
                            (string-match "^import .* from [\"']react[\"']" (buffer-string))))
                 . rjsx-mode))
  :config
  (add-hook 'rjsx-mode-hook (lambda () (setq emmet-expand-jsx-className? t))))

(use-package add-node-modules-path
  :config
  (eval-after-load 'js2-mode
    '(add-hook 'js-mode-hook #'add-node-modules-path))
  (eval-after-load 'rjsx-mode
    '(add-hook 'js-mode-hook #'add-node-modules-path)))
