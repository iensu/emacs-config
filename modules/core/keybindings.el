;;; modules/core/keybindings.el --- Global keybindings -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defun iensu/duplicate-line (n)
  "Copy the current line N times and insert it below."
  (interactive "P")
  (let ((cur-pos (point)))
    (dotimes (i (prefix-numeric-value n))
      (move-beginning-of-line nil)
      (kill-line)
      (yank)
      (newline)
      (insert (s-trim-right (car kill-ring)))
      (goto-char cur-pos))))

(defun iensu/toggle-scratch-buffer ()
  "Based on a great idea from Eric Skoglund (https://github.com/EricIO/emacs-configuration/)."
  (interactive)
  (if (string-equal (buffer-name (current-buffer))
                    "*scratch*")
      (switch-to-buffer (other-buffer))
    (switch-to-buffer "*scratch*")))

(defun iensu/move-file (new-location)
  "Write this file to NEW-LOCATION, and delete the old one.  Copied from http://zck.me/emacs-move-file."
  (interactive (list (if buffer-file-name
                         (read-file-name "Move file to: ")
                       (read-file-name "Move file to: "
                                       default-directory
                                       (expand-file-name (file-name-nondirectory (buffer-name))
                                                         default-directory)))))
  (when (file-exists-p new-location)
    (delete-file new-location))
  (let ((old-location (buffer-file-name)))
    (write-file new-location t)
    (when (and old-location
               (file-exists-p new-location)
               (not (string-equal old-location new-location)))
      (delete-file old-location))))

(defun iensu/switch-to-minibuffer ()
  "Switch to minibuffer window (if active)."
  (interactive)
  (when (active-minibuffer-window)
    (select-frame-set-input-focus (window-frame (active-minibuffer-window)))
    (select-window (active-minibuffer-window))))

(global-set-key (kbd "C-h C-s") 'iensu/toggle-scratch-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-<backspace>") 'delete-indentation)
(global-set-key (kbd "H-d") 'iensu/duplicate-line)
(global-set-key (kbd "H-f") 'counsel-projectile-find-file)
(global-set-key (kbd "H-m") 'iensu/move-file)
(global-set-key (kbd "H-o") 'iensu/switch-to-minibuffer)
(global-set-key (kbd "H-p") 'counsel-projectile-ag)
(global-set-key (kbd "H-t") 'toggle-truncate-lines)
(global-set-key (kbd "H-u") 'revert-buffer)
(global-set-key (kbd "H-x") 'yas-expand)
(global-set-key (kbd "M-i") 'imenu)
(global-set-key (kbd "M-o") 'occur)
(global-set-key [remap dabbrev-expand] 'hippie-expand)
