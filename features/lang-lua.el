(require 'lua-mode)

(add-to-list 'auto-mode-alist '("\\.lua$"  . lua-mode))

(require 'lsp)

(defun iensu--lua-mode-hook ()
  (lsp-deferred))

(add-hook 'lua-mode-hook #'iensu--lua-mode-hook)

(bind-key (kbd "C-c C-c") #'compile 'lua-mode-map)
