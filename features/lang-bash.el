(require 'sh-script)

(add-to-list 'auto-mode-alist '("\\.sh$" . bash-ts-mode))

(use-package flymake-shellcheck
  :ensure t
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))
