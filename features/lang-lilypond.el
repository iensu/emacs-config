;; Can't get lilypond to load reliably with use-package and straight...
(add-to-list 'load-path (expand-file-name "packages/lilypond" user-emacs-directory))
(autoload 'LilyPond-mode "lilypond-mode" "LilyPond Editing Mode" t)
(add-to-list 'auto-mode-alist '("\\.ly\\'" . LilyPond-mode))
(add-to-list 'auto-mode-alist '("\\.ily\\'" . LilyPond-mode))
