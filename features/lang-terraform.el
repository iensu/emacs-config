(use-package terraform-mode
  :config
  (defun iensu--terraform-format ()
    (when (executable-find "terraform")
      (let ((fname (buffer-file-name)))
        (when (file-exists-p fname)
          (shell-command (format "terraform fmt %s" fname))
          (revert-buffer nil t)))))

  (add-hook 'terraform-mode-hook
            (lambda ()
              (add-hook 'after-save-hook #'iensu--terraform-format nil 'local))))
