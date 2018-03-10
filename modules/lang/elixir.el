;;; modules/lang/elixir.el --- Elixir

;;; Code:

(defun iensu/-elixir-mode-hook ()
  (setq prettify-symbols-alist '(("->" . ?→)
                                 ("<-" . ?←)
                                 ("|>" . ?▶)
                                 ("<|" . ?◀))))

(use-package elixir-mode
  :delight
  (elixir-mode "仙薬" :major)
  :config
  (add-hook 'elixir-mode-hook 'iensu/-elixir-mode-hook))

(use-package alchemist
  :delight
  (alchemist-mode " 錬")
  (alchemist-phoenix-mode " 鳳"))
