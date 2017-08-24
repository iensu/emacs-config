;;; lang/elm.el --- Elm

(use-package elm-mode
  :ensure t
  :init
  (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
  (add-hook 'elm-mode-hook
            (lambda ()
              (setq company-backends '(company-elm))))
  (add-hook 'elm-mode-hook
            (lambda ()
              (setq prettify-symbols-alist '(("->" . ?→)
                                             ("<-" . ?←)
                                             ("|>" . ?▶)
                                             ("<|" . ?◀)))))
  :config
  (setq elm-indent-offset 2
        elm-tags-exclude-elm-stuff nil
        elm-tags-on-save t)
  (when (executable-find "elm-format")
    (setq elm-format-on-save t)))

(use-package flycheck-elm
  :ensure t
  :init
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-elm-setup)))
