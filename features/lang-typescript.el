(require 'typescript-ts-mode)

(add-to-list 'auto-mode-alist '("\\.ts$"  . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx$" . tsx-ts-mode))

(defun iensu--typescript-mode-hook ()
    (add-node-modules-path)
    (rainbow-mode 1)
    (prettier-js-mode 1)
    (eglot-ensure))

(add-hook 'typescript-ts-mode-hook #'iensu--typescript-mode-hook)
(add-hook 'tsx-ts-mode-hook #'iensu--typescript-mode-hook)

(defun iensu/tsx-ts-mark-node ()
  (interactive)
  (let ((node (treesit-node-parent (treesit-node-at (point)))))
    (push-mark (treesit-node-end node) nil t)))

(defalias 'mn 'iensu/tsx-ts-mark-node)
