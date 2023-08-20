(require 'yaml-ts-mode)

(use-package highlight-indentation)

(defun iensu--yaml-mode-hook ()
  (display-line-numbers-mode 1)
  (flyspell-mode-off)
  (visual-line-mode -1)
  (visual-fill-column-mode -1)
  (highlight-indentation-mode 1))

(add-hook 'yaml-ts-mode-hook #'iensu--yaml-mode-hook)
