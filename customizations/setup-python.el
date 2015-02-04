;;;;;;;;;;
;; Python
;;;;;;;;;;

;; Run the following globally:
;;  pip install elpy jedi rope flake8 magicimport 

;; M-x pyenv-activate <RET> choose your venv

(elpy-enable)
(define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)
(define-key global-map (kbd "C-c o") 'iedit-mode)

; (setq elpy-rpc-backend "jedi")
(setq elpy-rpc-backen "rope")

(elpy-use-ipython)

(defun custom-python-indentation-hook ()
  (setq indent-tabs-mode nil)
  (setq python-indent 4)
  (setq tab-width 4))

(add-hook 'python-mode-hook 'custom-python-indentation-hook)
