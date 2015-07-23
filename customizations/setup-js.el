;; javascript / html
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode ))
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))

(add-hook 'js-mode-hook (lambda ()
                          (progn 
                            (js2-minor-mode t)
                            (my-paredit-nonlisp)
                            (subword-mode t)
                            (tern-mode t))))
(add-hook 'js2-mode-hook (lambda ()
                           (progn 
                             (ac-js2-mode t))))
(add-hook 'js2-mode-hook #'js2-reactor-mode)

(add-hook 'html-mode-hook 'subword-mode)
;; (eval-after-load "sgml-mode"
;;   '(progn
;;      (require 'tagedit)
;;      (tagedit-add-paredit-like-keybindings)
;;      (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))))
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(defun my-web-mode-hook ()
  "Hooks for web mode"
  (setq web-mode-markup-indent-offset 4))
(add-hook 'web-mode-hook 'my-web-mode-hook)

(eval-after-load 'tern
  '(progn 
     (require 'tern-auto-complete)
     (tern-ac-setup)))

(defun delete-tern-process ()
  (interactive)
  (delete-process "Tern"))

(setq js2-highlight-level 3)
(setq js-indent-level 4)

(eval-after-load 'js 
  '(progn
     (define-key js-mode-map "{" 'paredit-open-curly)
     (define-key js-mode-map "}" 'paredit-close-curly-and-newline)
     (define-key js-mode-map "[" 'paredit-open-bracket)
     (define-key js-mode-map "]" 'paredit-close-bracket)
     (define-key js-mode-map "(" 'paredit-open-angled)
     (define-key js-mode-map ")" 'paredit-close-parenthesis)
     (define-key js-mode-map "\"" 'paredit-doublequote)
     (electric-pair-mode t)))
