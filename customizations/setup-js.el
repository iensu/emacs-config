(require 'web-mode)
(require 'cl)
(require 'json-mode)
(require 'flycheck)

(add-hook 'after-init-hook #'global-flycheck-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

;; use eslint with web-mode for jsx files, requires global eslint!!!
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-mode 'javascript-eslint 'js2-mode)

;; javascript / html
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode ))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.dust$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.handlebars$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.styl$" . css-mode))

(defun js-autoinsert-semicolons ()
  (interactive)
  (let ((buf (current-buffer))
        (cur-pos (point))
        (missing-semicolon? (lambda (err)
                              (let ((type (caar err)))
                                (equal type "msg.missing.semi"))))
        (insert-semicolon (lambda (pos)
                            (goto-char pos)
                            (insert ";"))))
    (dolist (e (remove-if-not missing-semicolon? (js2-errors-and-warnings)))
     (goto-char (+ (cadr e) (caddr e)))
     (insert ";"))
    (goto-char cur-pos)))

(add-hook 'js2-mode-hook (lambda ()
                           (subword-mode)
                           (tern-mode)
                           (setq electric-indent-mode t)
                           (local-set-key (kbd "C-c ;") 'js-autoinsert-semicolons)
                           (push '("function" . ?λ) prettify-symbols-alist)))

(eval-after-load 'js
  '(progn
     (electric-pair-mode t)))

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(setq js2-basic-offset 2)
(setq js2-highlight-level 3)
(setq js2-indent-switch-body t)

;; html-mode
(add-hook 'html-mode-hook 'subword-mode)

(add-hook 'web-mode-hook (lambda ()
                           (setq web-mode-css-indent-offset 2)
                           (setq web-mode-code-indent-offset 2)
                           (setq web-mode-markup-indent-offset 2)))

;; Tern
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))

(defun delete-tern-process ()
  (interactive)
  (delete-process "Tern"))
