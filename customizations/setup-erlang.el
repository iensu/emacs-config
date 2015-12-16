(add-to-list 'auto-mode-alist '("\\.erl?$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.hrl?$" . erlang-mode))

(defun my-erlang-hook ()
  (require 'edts-start)
  (setq erlang-root-dir "/usr/lib/erlang")
  (add-to-list 'exec-path "/usr/lib/erlang/bin")
  (setq erlang-man-root-dir "/usr/lib/erlang/man")
  (setq edts-mode t))

(add-hook 'erlang-mode-hook 'my-erlang-hook)
