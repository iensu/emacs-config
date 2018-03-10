;;; modules/core/autocompletion.el --- AC config

;;; Code

(defun iensu/company-private-last-transformer (candidates)
  "Resort CANDIDATES putting private-ish properties last."
  (let* ((is-private #'(lambda (c) (or (string-prefix-p "_" c)
                                      (string-prefix-p "._" c))))
         (private (cl-remove-if-not '(lambda (c) (funcall is-private c)) candidates))
         (not-private (cl-remove-if '(lambda (c) (funcall is-private c)) candidates)))
    (append not-private private)))

(use-package company
  :delight
  :init (global-company-mode)
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-auto-complete t
        company-tooltip-align-annotations t
        company-dabbrev-downcase nil
        company-auto-complete-chars nil)
  (add-hook 'emacs-lisp-mode-hook (lambda ()
                                    (add-to-list 'company-backends 'company-elisp)))
  (add-to-list 'company-transformers 'iensu/company-private-last-transformer)
  (eval-after-load 'company (company-quickhelp-mode 1)))

(use-package company-quickhelp
  :config
  (setq company-quickhelp-delay 1)
  (define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin))

(use-package yasnippet
  :delight yas-minor-mode
  :init
  (yas-global-mode 1)
  (setq yas-snippet-dirs (add-to-list 'yas-snippet-dirs (iensu--config-file "snippets")))
  :config
  (add-hook 'snippet-mode-hook (lambda ()
                                 (setq mode-require-final-newline nil
                                       require-final-newline nil))))
