;;; lang/elm.el --- Elm


(defun iensu/-elm-mode-hook ()
  (elm-oracle-setup-completion)
  (setq company-backends '(company-elm)
        prettify-symbols-alist '(("->" . ?→)
                                 ("<-" . ?←)
                                 ("|>" . ?▶)
                                 ("<|" . ?◀))))

(use-package elm-mode
  :delight
  (elm "榆" :major)
  (elm-indent-mode "")
  :init
  (add-hook 'elm-mode-hook 'iensu/-elm-mode-hook)
  :config
  (setq elm-indent-offset 2
        elm-tags-exclude-elm-stuff nil
        elm-tags-on-save t)
  (when (executable-find "elm-format")
    (setq elm-format-on-save t)))

(use-package flycheck-elm
  :init
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-elm-setup)))
