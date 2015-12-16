;; Enable paredit for Clojure
(add-hook 'clojure-mode-hook 'enable-paredit-mode)

(add-hook 'adoc-mode-hook 'cider-mode)
(defun increment-clojure-cookbook ()
  "When reading the Clojure cookbook, find the next section, and
close the buffer. If the next section is a sub-directory or in
the next chapter, open Dired so you can find it manually."
  (interactive)
  (let* ((cur (buffer-name))
     (split-cur (split-string cur "[-_]"))
     (chap (car split-cur))
     (rec (car (cdr split-cur)))
     (rec-num (string-to-number rec))
     (next-rec-num (1+ rec-num))
     (next-rec-s (number-to-string next-rec-num))
     (next-rec (if (< next-rec-num 10)
               (concat "0" next-rec-s)
             next-rec-s))
     (target (file-name-completion (concat chap "-" next-rec) "")))
    (progn
      (if (equal target nil)
      (dired (file-name-directory (buffer-file-name)))
    (find-file target))
      (kill-buffer cur))))

(add-hook 'clojure-mode-hook 'subword-mode)
(add-hook 'clojure-mode-hook 'prettify-symbols-mode)

(require 'clojure-mode-extra-font-locking)

(add-hook 'clojure-mode-hook
          (lambda ()
            (setq inferior-lisp-program "lein repl")
            (font-lock-add-keywords
             nil
             '(("(\\(facts?\\)"
                (1 font-lock-keyword-face))
               ("(\\(background?\\)"
                (1 font-lock-keyword-face))))
            (define-clojure-indent (fact 1))
            (define-clojure-indent (facts 1))))

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

(setq cider-repl-pop-to-buffer-on-connect t)
(setq cider-show-error-buffer t)
(setq cider-auto-select-error-buffer nil)
(setq cider-repl-history-file "~/.emacs.d/cider-history")
(setq cider-repl-wrap-history t)

(add-hook 'cider-repl-mode-hook 'paredit-mode)

(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))

(defun cider-start-http-server ()
  (interactive)
  (cider-load-current-buffer)
  (let ((ns (cider-current-ns)))
    (cider-repl-set-ns ns)
    (cider-interactive-eval (format "(println '(def server (%s/start))) (println 'server)" ns))
    (cider-interactive-eval (format "(def server (%s/start)) (println server)" ns))))

(defun cider-refresh ()
  (interactive)
  (cider-interactive-eval (format "(user/reset)")))

(defun cider-user-ns ()
  (interactive)
  (cider-repl-set-ns "user"))

(defun cider-figwheel-repl ()
  (interactive)
  (save-some-buffers)
  (with-current-buffer (cider-current-repl-buffer)
    (goto-char (point-max))
    (insert "(require 'figwheel-sidecar.repl-api)
             (figwheel-sidecar.repl-api/start-figwheel!
               {:all-builds (figwheel-sidecar.repl/get-project-cljs-builds)})
             (figwheel-sidecar.repl-api/cljs-repl)")
    (cider-repl-return)))

(eval-after-load 'cider
  '(progn
     (define-key clojure-mode-map (kbd "C-c C-v") 'cider-start-http-server)
     (define-key clojure-mode-map (kbd "C-M-r") 'cider-refresh)
     (define-key clojure-mode-map (kbd "C-c u") 'cider-user-ns)
     (define-key cider-mode-map (kbd "C-c u") 'cider-user-ns)
     (define-key cider-mode-map (kbd "C-c C-f") 'cider-figwheel-repl)))
