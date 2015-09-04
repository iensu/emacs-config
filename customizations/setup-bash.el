(defun bash-mode-setup-hook ()
  (setq sh-basic-offset 2)
  (setq sh-indentation 2))

(add-hook 'sh-mode-hook 'bash-mode-setup-hook)
