(require 'web-mode)
(require 'cl)

;; javascript / html
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode ))
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
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

;; js-mode
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js-mode-hook 'subword-mode)
(add-hook 'js-mode-hook 'tern-mode)
(add-hook 'js-mode-hook 'electric-indent-mode)
(add-hook 'js-mode-hook (lambda ()
                          (local-set-key (kbd "C-c ;") 'js-autoinsert-semicolons)
                          (push '("function" . ?λ) prettify-symbols-alist)))

(eval-after-load 'js
  '(progn
     (electric-pair-mode t)))

(setq js-indent-level 2)

;; js2-mode
;; (add-hook 'js2-mode-hook (lambda ()
;;                            (progn
;;                              (ac-js2-mode t))))

(add-hook 'js2-mode-hook #'js2-refactor-mode)

(setq js2-highlight-level 3)

;; html-mode
(add-hook 'html-mode-hook 'subword-mode)

(add-hook 'web-mode-hook (lambda ()
                           (setq web-mode-markup-indent-offset 2)))

;; Tern
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))

(defun delete-tern-process ()
  (interactive)
  (delete-process "Tern"))
