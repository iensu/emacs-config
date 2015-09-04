(require 'web-mode)
(require 'flycheck)
(require 'cl)

;; javascript / html
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode ))
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.dust$" . web-mode))

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
(add-hook 'js-mode-hook (lambda ()
                          (progn 
                            (js2-minor-mode t)
                            (subword-mode t)
                            (tern-mode t)
                            (electric-indent-mode +1)
                            (local-set-key (kbd "C-c ;") 'js-autoinsert-semicolons))))

(eval-after-load 'js 
  '(progn
     (electric-pair-mode t)))

(setq js-indent-level 2)

;; js2-mode
(add-hook 'js2-mode-hook (lambda ()
                           (progn 
                             (ac-js2-mode t))))

(add-hook 'js2-mode-hook #'js2-reactor-mode)

(setq js2-highlight-level 3)

;; html-mode
(add-hook 'html-mode-hook 'subword-mode)

;; jsx settings
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

(flycheck-define-checker jsxhint-checker
  "A JSX syntax and style checker based on JSXHint."

  :command ("jsxhint" source)
  :error-patterns
  ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
  :modes (web-mode))

(add-hook 'web-mode-hook
          (lambda ()
            (when (equal web-mode-content-type "jsx")
              ;; enable flycheck
              (flycheck-select-checker 'jsxhint-checker)
              (flycheck-mode))))

(defun my-web-mode-hook ()
  "Hooks for web mode"
  (setq web-mode-markup-indent-offset 2))
(add-hook 'web-mode-hook 'my-web-mode-hook)

;; Tern
(eval-after-load 'tern
  '(progn 
     (require 'tern-auto-complete)
     (tern-ac-setup)))

(defun delete-tern-process ()
  (interactive)
  (delete-process "Tern"))
