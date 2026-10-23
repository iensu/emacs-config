;; -*- lexical-binding: t; -*-

(use-package terraform-mode
  :ensure t
  :vc (terraform-mode :url "https://github.com/hcl-emacs/terraform-mode"
                      :rev "01635df3625c0cec2bb4613a6f920b8569d41009")
  :config
  (defun iensu--terraform-format ()
    (when (executable-find "terraform")
      (let ((fname (buffer-file-name)))
        (when (file-exists-p fname)
          (shell-command (format "terraform fmt %s" fname))
          (revert-buffer nil t))))
    (when (executable-find "tofu")
      (let ((fname (buffer-file-name)))
        (when (file-exists-p fname)
          (shell-command (format "tofu fmt %s" fname))
          (revert-buffer nil t)))))

  (add-hook 'terraform-mode-hook
            (lambda ()
              (add-hook 'after-save-hook #'iensu--terraform-format nil 'local))))
