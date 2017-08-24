;;; lang/elixir.el --- Elixir

(use-package elixir-mode
  :ensure t
  :config
  (add-hook 'elixir-mode-hook
            (lambda ()
              (setq prettify-symbols-alist (("->" . ?→)
                                            ("<-" . ?←)
                                            ("|>" . ?▶)
                                            ("<|" . ?◀))))))

(use-package alchemist :ensure t)
