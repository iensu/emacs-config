(defconst rlw-java-style 
          '((c-basic-offset . 2) 
            (c-comment-only-line-offset . (0 . 0)) 
            (c-comment-continuation-stars . "") 
            (c-hanging-comment-starter-p . nil) 
            (c-hanging-comment-ender-p . nil) 
            (c-cleanup-list . ( 
                               list-close-comma 
                               )) 
            (c-offsets-alist . ( 
                                (arglist-intro . c-lineup-arglist-intro-after-paren) 
                                (arglist-close . c-lineup-arglist) 
                                (case-label . *) 
                                (func-decl-cont . c-lineup-java-throws) 
                                (inher-cont . c-lineup-java-inher) 
                                (inline-open . 0) 
                                (statement-case-open . +) 
                                (statement-cont . *) 
                                (substatement-open . 0) 
                                )) 
            (c-hanging-braces-alist . ( 
                                       (block-close . c-snug-do-while) 
                                       (block-open after) 
                                       (class-open after) 
                                       (defun-open after) 
                                       (inline-open after) 
                                       (substatement-open after) 
                                       )) 
            (c-hanging-colons-alist . ( 
                                       (case-label after) 
                                       )) 
            ) 
          "RLW Java Programming Style")

(defun rlw-java-mode-hook () 
  (c-add-style "RLW" rlw-java-style t) 
  (setq tab-width 4 
        indent-tabs-mode nil 
        c-indent-comments-syntactically-p t) 
  (c-toggle-auto-hungry-state 1) 
  ) 
        
(add-hook 'java-mode-hook 'rlw-java-mode-hook) 
                
(defun rlw-groovy-mode-hook () 
  (c-add-style "RLW" rlw-java-style t) 
  (setq tab-width 4 
        indent-tabs-mode nil 
        c-indent-comments-syntactically-p t) 
  (c-toggle-auto-hungry-state 1) 
  ) 
        
(add-hook 'groovy-mode-hook 'rlw-groovy-mode-hook) 


;; (add-hook 'groovy-mode-hook
;; 	  '(lambda ()
;; 	     (require 'groovy-electric)
;; 	     (groovy-electric-mode)))
