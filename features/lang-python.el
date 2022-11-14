(add-hook 'python-mode-hook 'eglot-ensure)

(use-package python-black
  :demand t
  :after python
  :config
  (add-hook 'python-mode-hook (lambda ()
                                (when (executable-find "black")
                                  (python-black-on-save-mode 1)))))
