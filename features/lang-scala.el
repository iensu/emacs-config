(use-package scala-mode
  :hook
  (scala-mode . eglot-ensure)
  :mode "\\.s\\(cala\\|bt\\)$")

(use-package sbt-mode
  :commands (sbt-start sbt-command)
  :custom
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (sbt:program-options '("-Dsbt.supershell=false"))
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))
