(add-hook 'groovy-mode-hook
          (lambda ()
            (c-set-offset 'label 4)
            (setq c-default-style "linux"
                  c-basic-offset 4)
            (autopair-mode t)))

