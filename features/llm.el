(use-package gptel
  :config
  (global-set-key (kbd "C-c RET") 'gptel-send)

  (defun iensu/gptel-refresh-models ()
    (interactive)

    (gptel-make-privategpt "LM Studio"
        :protocol "http"
        :host "localhost:1234"
        :stream t
        :context t
        :sources t
        :models '(openai/gpt-oss-20b))

    (gptel-make-ollama "Ollama"
    :host "localhost:11434"
    :stream t
    :models (mapcar (lambda (line)
                      (car (string-split line "\s+")))
                    (drop 1
                          (string-split (string-trim (shell-command-to-string "ollama list")) "\n")))))

  (iensu/gptel-refresh-models)

  ;; Tools
  (gptel-make-tool
   :name "list_directory"
   :function (lambda (path)
               (let ((default-directory path))
                 (shell-command-to-string "ls -l")))
   :description "List the contents of a specified directory"
   :args (list '(:name "path"
	                     :type string
	                     :description "The directory to list"))
   :category "filesystem")
  (gptel-make-tool
   :name "create_file"
   :function (lambda (path filename content)
               (let ((full-path (expand-file-name filename path)))
                 (with-temp-buffer
                   (insert content)
                   (write-file full-path))
                 (format "Created file %s in %s" filename path)))
   :description "Create a new file with the specified content"
   :args (list '(:name "path"
	                     :type string
	                     :description "The directory where to create the file")
               '(:name "filename"
	                     :type string
	                     :description "The name of the file to create")
               '(:name "content"
	                     :type string
	                     :description "The content to write to the file"))
   :category "filesystem")
  (gptel-make-tool
   :name "read_file"
   :function (lambda (filepath)
               (with-temp-buffer
                 (insert-file-contents filepath)
                 (buffer-string)))
   :description "Read the contents of a specified file"
   :args (list '(:name "filepath"
	                     :type string
	                     :description "The path to the file"))
   :category "filesystem")

  (setopt gptel-model 'gpt-oss:20b))
