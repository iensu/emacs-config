;;; org-cryptage.el --- Public Key Encryption for Org Entries -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2007-2026 Free Software Foundation, Inc.
;; Copyright (C) 2026 Jens Östlund

;; Author: Jens Östlund <hi@iensu.me>

;; This file is a derivative of org-crypt.el, which is part of GNU
;; Emacs; it is not itself part of GNU Emacs and is not distributed
;; with it.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package builds on org-crypt.el (part of GNU Emacs/Org mode,
;; originally by John Wiegley), reworked to use Age
;; (https://github.com/FiloSottile/age) instead of GnuPG.  It shells
;; out to the `age' command line tool directly and has no dependency
;; on any third-party Emacs package -- only `age' (or a compatible
;; implementation, e.g. `rage') needs to be installed and on
;; `exec-path'.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-macs)
(require 'org-compat)
(require 'subr-x)

(declare-function org-back-over-empty-lines "org" ())
(declare-function org-current-level "org" ())
(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-before-first-heading-p "org" ())
(declare-function org-end-of-meta-data "org" (&optional full))
(declare-function org-end-of-subtree "org" (&optional invisible-ok to-heading element))
(declare-function org-entry-get "org" (pom property &optional inherit literal-nil))
(declare-function org-fold-subtree "org-fold" (flag))
(declare-function org-make-tags-matcher "org" (match &optional only-local-tags))
(declare-function org-previous-visible-heading "org" (arg))
(declare-function org-scan-tags "org" (action matcher todo-only &optional start-level))
(declare-function org-cycle-set-startup-visibility "org-cycle" ())

(defgroup org-cryptage nil
  "Org Crypt."
  :tag "Org Crypt"
  :group 'org)

(defcustom org-cryptage-program (executable-find "age")
  "The Age executable used to encrypt and decrypt entries.

Any command line compatible implementation works, e.g. `age' or
`rage'."
  :group 'org-cryptage
  :type '(choice (file :tag "Path to executable")
		 (const :tag "Not found" nil)))

(defcustom org-cryptage-tag-matcher "crypt"
  "The tag matcher used to find headings whose contents should be encrypted.

See the \"Match syntax\" section of the org manual for more details."
  :type 'string
  :group 'org-cryptage)

(defcustom org-cryptage-key nil
  "The default recipient to use when encrypting the contents of a heading.

This is either an Age public key (starting with \"age1\") or a path
to a file containing one recipient per line.

If this variable is nil, always use symmetric (passphrase)
encryption, unconditionally.

This setting can be overridden in the CRYPTKEY property."
  :group 'org-cryptage
  :type '(choice
	  (string :tag "Age public key or path to recipients file")
	  (const :tag "Symmetric encryption" nil)))

(defcustom org-cryptage-identity-file nil
  "Path to the Age identity (private key) file used to decrypt entries.

This is only used to decrypt entries that were encrypted for a
recipient; it plays no role in symmetric (passphrase) encryption."
  :group 'org-cryptage
  :type 'file)

(defcustom org-cryptage-disable-auto-save 'ask
  "What org-decrypt should do if `auto-save-mode' is enabled.

t        : Disable `auto-save-mode' for the current buffer
           prior to decrypting an entry.

nil      : Leave `auto-save-mode' enabled.
           This may cause data to be written to disk unencrypted!

`ask'    : Ask user whether or not to disable `auto-save-mode'
           for the current buffer.

`encrypt': Leave `auto-save-mode' enabled for the current buffer,
           but automatically re-encrypt all decrypted entries
           *before* auto-saving.
           NOTE: This only works for entries which have a tag
           that matches `org-cryptage-tag-matcher'."
  :group 'org-cryptage
  :version "24.1"
  :type '(choice (const :tag "Always"  t)
                 (const :tag "Never"   nil)
                 (const :tag "Ask"     ask)
                 (const :tag "Encrypt" encrypt)))

;;; Low-level Age process handling

(defconst org-cryptage--armor-regexp
  "-----BEGIN AGE ENCRYPTED FILE-----\n\\(?:.\\|\n\\)*?-----END AGE ENCRYPTED FILE-----\n"
  "Regexp matching a complete ASCII-armored Age message.")

(defconst org-cryptage--csi-regexp "\e\\[[0-9;]*[A-Za-z]"
  "Regexp matching a single ANSI CSI escape sequence.

Age emits these to erase the passphrase it echoes on its
controlling terminal; they need to be scrubbed out of anything it
writes back to us over a pty.")

(defun org-cryptage--program ()
  "Return `org-cryptage-program', erroring out if unset."
  (or org-cryptage-program
      (error "Could not find the `age' executable; set `org-cryptage-program'")))

(defun org-cryptage--recipient-args (recipients)
  "Turn RECIPIENTS into `age' command line arguments.
RECIPIENTS is a list of Age public keys and/or paths to files
listing recipients, one per line."
  (mapcan
   (lambda (recipient)
     (if (file-exists-p (expand-file-name recipient))
	 (list "-R" (expand-file-name recipient))
       (list "-r" recipient)))
   recipients))

(defun org-cryptage--call (args input)
  "Run the Age executable with ARGS, feeding it INPUT on stdin.
INPUT and the returned string are treated as raw bytes.  Signal an
error including standard error if the process exits non-zero."
  (let ((errfile (make-temp-file "org-cryptage-err")))
    (unwind-protect
	(with-temp-buffer
	  (set-buffer-multibyte nil)
	  (let ((status (apply #'call-process-region
				input nil (org-cryptage--program) nil
				(list t errfile) nil args)))
	    (if (eq status 0)
		(buffer-string)
	      (error "age failed: %s"
		     (string-trim
		      (with-temp-buffer
			(insert-file-contents errfile)
			(buffer-string)))))))
      (ignore-errors (delete-file errfile)))))

(defun org-cryptage--run-pty (args on-ready)
  "Run the Age executable with ARGS connected via a pseudo-terminal.
Age insists on reading and writing passphrase prompts on a
controlling terminal rather than plain stdin/stdout, so symmetric
encryption needs a pty to be scriptable at all.

ON-READY is called once with the live process, and is responsible
for sending it input via `process-send-string'/`process-send-eof'.
Return everything the process wrote, once it has exited."
  (let* ((output "")
	 (proc (make-process
		:name "org-cryptage-age"
		:command (cons (org-cryptage--program) args)
		:connection-type 'pty
		:noquery t
		:filter (lambda (_proc string)
			  (setq output (concat output string))))))
    (unwind-protect
	(progn
	  (accept-process-output proc 1)
	  (funcall on-ready proc)
	  (while (process-live-p proc)
	    (accept-process-output proc 1)))
      (when (process-live-p proc)
	(delete-process proc)))
    output))

(defun org-cryptage--encrypt-recipients (plain recipients)
  "Encrypt PLAIN (a string) for RECIPIENTS.
RECIPIENTS is a non-empty list as returned by
`org-cryptage-key-for-heading'.  Return the ASCII-armored
ciphertext."
  (org-cryptage--call
   (append '("--encrypt" "--armor") (org-cryptage--recipient-args recipients))
   (encode-coding-string plain 'utf-8)))

(defun org-cryptage--decrypt-identity (cipher identity-file)
  "Decrypt CIPHER (ASCII-armored ciphertext) using IDENTITY-FILE."
  (decode-coding-string
   (org-cryptage--call
    (list "--decrypt" "-i" (expand-file-name identity-file))
    (encode-coding-string cipher 'utf-8))
   'utf-8))

(defun org-cryptage--encrypt-passphrase (plain passphrase)
  "Symmetrically encrypt PLAIN with PASSPHRASE.
Return the ASCII-armored ciphertext."
  (let ((output
	 (org-cryptage--run-pty
	  '("--encrypt" "--armor" "--passphrase")
	  (lambda (proc)
	    (process-send-string proc (concat passphrase "\n"))
	    (accept-process-output proc 0.3)
	    (process-send-string proc (concat passphrase "\n"))
	    (accept-process-output proc 0.3)
	    (process-send-string proc (encode-coding-string plain 'utf-8))
	    (process-send-eof proc)))))
    (if (string-match org-cryptage--armor-regexp output)
	(match-string 0 output)
      (error "age encryption failed: %s"
	     (string-trim (replace-regexp-in-string
			   org-cryptage--csi-regexp "" output))))))

(defun org-cryptage--decrypt-passphrase (cipher passphrase)
  "Symmetrically decrypt CIPHER (ASCII-armored ciphertext) with PASSPHRASE."
  (let* ((cipher-file (make-temp-file "org-cryptage-cipher"))
	 (output
	  (unwind-protect
	      (progn
		(let ((coding-system-for-write 'binary))
		  (write-region cipher nil cipher-file nil 'quiet))
		(org-cryptage--run-pty
		 (list "--decrypt" cipher-file)
		 (lambda (proc)
		   (process-send-string proc (concat passphrase "\n")))))
	    (ignore-errors (delete-file cipher-file))))
	 (cleaned (replace-regexp-in-string org-cryptage--csi-regexp "" output)))
    (if (string-match "\\`Enter passphrase:[ \t]*\r?\n" cleaned)
	(decode-coding-string
	 (encode-coding-string (substring cleaned (match-end 0)) 'raw-text)
	 'utf-8)
      (error "age decryption failed: %s" (string-trim cleaned)))))

(defun org-cryptage--encrypt-string (plain recipients)
  "Encrypt PLAIN for RECIPIENTS, or symmetrically if RECIPIENTS is nil."
  (if recipients
      (org-cryptage--encrypt-recipients plain recipients)
    (org-cryptage--encrypt-passphrase
     plain (read-passwd "Passphrase to encrypt entry: " t))))

(defun org-cryptage--scrypt-p (cipher)
  "Return non-nil if the armored Age message CIPHER uses a passphrase."
  (with-temp-buffer
    (insert cipher)
    (goto-char (point-min))
    (when (looking-at-p "-----BEGIN AGE ENCRYPTED FILE-----")
      (forward-line 1)
      (let ((line (buffer-substring-no-properties
		   (line-beginning-position) (line-end-position))))
	(string-match-p "-> scrypt "
			(or (ignore-errors (base64-decode-string line)) ""))))))

(defun org-cryptage--decrypt-string (cipher)
  "Decrypt CIPHER, using `org-cryptage-identity-file' or a passphrase."
  (if (org-cryptage--scrypt-p cipher)
      (org-cryptage--decrypt-passphrase
       cipher (read-passwd "Passphrase to decrypt entry: "))
    (org-cryptage--decrypt-identity cipher org-cryptage-identity-file)))

;;; Org integration

(defun org-cryptage--encrypted-text (beg end)
  "Return encrypted text in between BEG and END."
  ;; Ignore indentation.
  (replace-regexp-in-string
   "^[ \t]*" ""
   (buffer-substring-no-properties beg end)))

(defun org-at-encrypted-entry-p ()
  "Is the current entry encrypted?
When the entry is encrypted, return a pair (BEG . END) where BEG
and END are buffer positions delimiting the encrypted area."
  (org-with-wide-buffer
   (unless (org-before-first-heading-p)
     (org-back-to-heading t)
     (org-end-of-meta-data 'standard)
     (let ((case-fold-search nil)
	   (banner-start (rx (seq bol
				  (zero-or-more (any "\t "))
				  "-----BEGIN AGE ENCRYPTED FILE-----"
				  eol))))
       (when (looking-at banner-start)
	 (let ((start (point))
	       (banner-end (rx (seq bol
				    (or (group (zero-or-more (any "\t "))
					       "-----END AGE ENCRYPTED FILE-----"
					       eol)
					(seq (one-or-more "*") " "))))))
	   (when (and (re-search-forward banner-end nil t) (match-string 1))
	     (cons start (line-beginning-position 2)))))))))

(defun org-cryptage-check-auto-save ()
  "Check whether `auto-save-mode' is enabled for the current buffer.

`auto-save-mode' may cause leakage when decrypting entries, so
check whether it's enabled, and decide what to do about it.

See `org-cryptage-disable-auto-save'."
  (when buffer-auto-save-file-name
    (cond
     ((or
       (eq org-cryptage-disable-auto-save t)
       (and
	(eq org-cryptage-disable-auto-save 'ask)
	(y-or-n-p "`org-decrypt': auto-save-mode may cause leakage.  Disable it for current buffer? ")))
      (message "org-decrypt: Disabling auto-save-mode for %s"
               (or (buffer-file-name) (current-buffer)))
      ;; The argument to auto-save-mode has to be "-1", since
      ;; giving a "nil" argument toggles instead of disabling.
      (auto-save-mode -1))
     ((eq org-cryptage-disable-auto-save nil)
      (message "org-decrypt: Decrypting entry with auto-save-mode enabled.  This may cause leakage."))
     ((eq org-cryptage-disable-auto-save 'encrypt)
      (message "org-decrypt: Enabling re-encryption on auto-save.")
      (add-hook 'auto-save-hook
		(lambda ()
		  (message "org-cryptage: Re-encrypting all decrypted entries due to auto-save.")
		  (org-encrypt-entries))
		nil t))
     (t nil))))

(defun org-cryptage-key-for-heading ()
  "Return the encryption recipient(s) for the current heading, or nil.

The result is a list of Age public keys/recipient files suitable
for passing to `org-cryptage--encrypt-string', or nil to fall back
to symmetric (passphrase) encryption."
  (when org-cryptage-key
    (let ((key (pcase (org-entry-get nil "CRYPTKEY" 'selective 'literal-nil)
		 ("nil" nil)
		 (prop (or prop org-cryptage-key)))))
      (when key (list key)))))

;;;###autoload
(defun org-encrypt-entry ()
  "Encrypt the content of the current headline."
  (interactive)
  (unless (org-at-encrypted-entry-p)
    (org-with-wide-buffer
     (org-back-to-heading t)
     (let ((start-heading (point))
	   (crypt-key (org-cryptage-key-for-heading))
	   (folded? (org-invisible-p (line-beginning-position))))
       (org-end-of-meta-data 'standard)
       (let ((beg (point))
	     (folded-heading
	      (and folded?
		   (save-excursion
		     (org-previous-visible-heading 1)
		     (point)))))
	 (goto-char start-heading)
	 (org-end-of-subtree t t)
	 (org-back-over-empty-lines)
	 (let* ((contents (delete-and-extract-region beg (point)))
		(key (get-text-property 0 'org-cryptage-key contents))
		(checksum (get-text-property 0 'org-cryptage-checksum contents)))
	   (condition-case err
	       (insert
		;; Text and key have to be identical, otherwise we
		;; re-crypt.
		(if (and (equal crypt-key key)
			 (string= checksum (sha1 contents)))
		    (get-text-property 0 'org-cryptage-text contents)
		  (org-cryptage--encrypt-string contents crypt-key)))
	     ;; If encryption failed, make sure to insert back entry
	     ;; contents in the buffer.
	     (error
	      (insert contents)
	      (error "%s" (error-message-string err)))))
	 (when folded-heading
	   (goto-char folded-heading)
	   (org-fold-subtree t))
	 nil)))))

(defvar org-outline-regexp-bol)
;;;###autoload
(defun org-decrypt-entry ()
  "Decrypt the content of the current headline."
  (interactive)
  (pcase (org-at-encrypted-entry-p)
    (`(,beg . ,end)
     (org-with-point-at beg
       (org-cryptage-check-auto-save)
       (let* ((folded-heading
	       (and (org-invisible-p)
		    (save-excursion
		      (org-previous-visible-heading 1)
		      (point))))
              (level (org-current-level))
	      (encrypted-text (org-cryptage--encrypted-text beg end))
	      (decrypted-text (org-cryptage--decrypt-string encrypted-text))
              origin-marker)
	 ;; Delete region starting just before point, because the
	 ;; outline property starts at the \n of the heading.
	 (delete-region (1- (point)) end)
         (setq origin-marker (point-marker))
         (if (string-match (org-headline-re level) decrypted-text)
             ;; If decrypted text contains other headings with levels
             ;; below LEVEL, adjust the subtree.
             (let ((start 0) (min-level level))
               (while (string-match (org-headline-re level) decrypted-text start)
                 (setq min-level (min min-level (1- (length (match-string 0 decrypted-text))))
                       start (match-end 0)))
               (insert "\n"
                       (replace-regexp-in-string
                        org-outline-regexp-bol
                        (concat (make-string (1+ (- level min-level)) ?*) "\\&")
                        decrypted-text)))
	   ;; Store a checksum of the decrypted and the encrypted text
	   ;; value.  This allows reusing the same encrypted text if the
	   ;; text does not change, and therefore avoid a re-encryption
	   ;; process.
	   (insert "\n"
		   (propertize decrypted-text
			       'org-cryptage-checksum (sha1 decrypted-text)
			       'org-cryptage-key (org-cryptage-key-for-heading)
			       'org-cryptage-text encrypted-text)))
         ;; Apply initial visibility.
         (save-restriction
           (narrow-to-region origin-marker (point))
           (set-marker origin-marker nil)
           (org-cycle-set-startup-visibility))
         ;; ... but keep the previous folded state.
	 (when folded-heading
	   (goto-char folded-heading)
	   (org-fold-subtree t))
	 nil)))
    (_ nil)))

(defvar org--matcher-tags-todo-only)

;;;###autoload
(defun org-encrypt-entries ()
  "Encrypt all top-level entries in the current buffer."
  (interactive)
  (let ((org--matcher-tags-todo-only nil))
    (org-scan-tags
     'org-encrypt-entry
     (cdr (org-make-tags-matcher org-cryptage-tag-matcher))
     org--matcher-tags-todo-only)))

;;;###autoload
(defun org-decrypt-entries ()
  "Decrypt all entries in the current buffer."
  (interactive)
  (let ((org--matcher-tags-todo-only nil))
    (org-scan-tags
     'org-decrypt-entry
     (cdr (org-make-tags-matcher org-cryptage-tag-matcher))
     org--matcher-tags-todo-only)))

;;;###autoload
(defun org-cryptage-use-before-save-magic ()
  "Add a hook to automatically encrypt entries before a file is saved to disk."
  (add-hook
   'org-mode-hook
   (lambda () (add-hook 'before-save-hook 'org-encrypt-entries nil t))))

(add-hook 'org-fold-reveal-start-hook 'org-decrypt-entry)

(provide 'org-cryptage)

;;; org-cryptage.el ends here
