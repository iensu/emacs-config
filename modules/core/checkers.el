;;; modules/core/checkers.el --- Spell checking, Linting etc

;;; Commentary:

;;; Code:

(use-package flyspell
  :delight
  :config
  (when (executable-find "aspell")
    (setq ispell-program-name "aspell"
          ispell-extra-args '("--sug-mode=ultra")
          ispell-list-command "--list")))

(use-package flyspell-popup
  :delight
  :bind
  (:map flyspell-mode-map
        ("C-;" . flyspell-popup-correct)))

(use-package flycheck
  :delight
  :bind
  (:map iensu-map
        ("e" . flycheck-list-errors))
  :init
  (global-flycheck-mode t)
  :config
  (flycheck-add-mode 'typescript-tslint 'web-mode))

(use-package flycheck-popup-tip
  :init
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode)))
