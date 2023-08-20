(require 'cmake-ts-mode)

(add-hook 'makefile-mode-hook
          (lambda ()
            (whitespace-mode 1)
            (indent-tabs-mode 1)))
