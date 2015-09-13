(add-hook 'geiser-mode-hook
          (lambda ()
            (print "hello there")
            (setq company-quickhelp-mode nil)))
