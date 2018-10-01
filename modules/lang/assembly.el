(defun iensu--asm-mode-hook ()
  (setq tab-stop-list (number-sequence 2 60 2))
  (setq-default asm-comment-char ?@)
  (electric-indent-mode -1))

(use-package asm
  :bind (:map asm-mode-map
              ("M-." . etags-goto-tag-location))
  :config
  (add-hook 'asm-mode-hook 'iensu--asm-mode-hook))
