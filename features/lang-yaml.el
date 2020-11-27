(use-package yaml-mode
  :hook
  (yaml-mode . display-line-numbers-mode)
  (yaml-mode . flyspell-mode-off)
  ;; YAML mode inherits from text-mode, so need to disable some settings
  (yaml-mode . (lambda ()
                 (visual-line-mode -1)
                 (visual-fill-column-mode -1))))

(use-package highlight-indentation :hook (yaml-mode . highlight-indentation-mode))
