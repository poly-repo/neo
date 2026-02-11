;; -*- lexical-binding: t; -*-

(require 'cl-lib)

;; (cl-defun neo/execute (command &key args on-success on-error buffer-name (show-buffer t))
;;   "Execute COMMAND with ARGS asynchronously.
;; If SHOW-BUFFER is nil, stay quiet unless an error occurs."
;;   (let ((buf (get-buffer-create (or buffer-name "*NEO Subprocess*")))
;;         (full-command (append (list command) args)))
;;     (with-current-buffer buf 
;;       (let ((inhibit-read-only t))
;;         (erase-buffer)
;;         (insert (format "NEO Executing: %s\n\n" (mapconcat #'identity full-command " ")))
;;         ;; Only display if the user wants it or if we are debugging
;;         (when show-buffer (display-buffer buf))))
;;     (make-process
;;      :name "neo-subtask"
;;      :buffer buf
;;      :command full-command
;;      :sentinel (lambda (proc _event)
;;                  (when (eq (process-status proc) 'exit)
;;                    (let ((status (process-exit-status proc))
;;                          (p-buf (process-buffer proc)))
;;                      (if (zerop status)
;;                          (progn
;;                            (when on-success (funcall on-success)))
;; 		       ;; On Error: Always show the buffer regardless of show-buffer setting
;; 		       (when p-buf (pop-to-buffer p-buf))
;; 		       (when on-error (funcall on-error)))))))))

;; (cl-defun neo/execute (command &key args env on-success on-error buffer-name (show-buffer t))
;;   "Execute COMMAND with ARGS asynchronously with optional ENV alist.
;; ENV should be an alist of (VAR . VALUE) strings."
;;   (let ((buf (get-buffer-create (or buffer-name "*NEO Subprocess*")))
;;         (full-command (append (list (expand-file-name command)) args))
;;         ;; Merge custom env into the existing Emacs environment
;;         (process-environment (append 
;;                               (mapcar (lambda (x) (format "%s=%s" (car x) (cdr x))) env)
;;                               process-environment)))
;;     (with-current-buffer buf 
;;       (let ((inhibit-read-only t))
;;         (erase-buffer)
;;         (insert (format "NEO Executing: %s\n" (mapconcat #'identity full-command " ")))
;;         (when env
;;           (insert (format "With ENV: %s\n" env)))
;;         (insert "\n")
;;         (when show-buffer (display-buffer buf))))
;;     (make-process
;;      :name "neo-subtask"
;;      :buffer buf
;;      :command full-command
;;      :sentinel (lambda (proc _event)
;;                  (when (eq (process-status proc) 'exit)
;;                    (let ((status (process-exit-status proc))
;;                          (p-buf (process-buffer proc)))
;;                      (if (zerop status)
;;                          (when on-success (funcall on-success))
;;                        (when p-buf (pop-to-buffer p-buf))
;;                        (when on-error (funcall on-error)))))))))


(cl-defun neo/execute (command &key args env on-success on-error buffer-name (show-buffer t) sync)
  "Execute COMMAND with ARGS. 
If SYNC is non-nil, wait for the process to finish before returning."
  (let ((buf (get-buffer-create (or buffer-name "*NEO Subprocess*")))
        (full-command (append (list (expand-file-name command)) args))
        (process-environment (append 
                              (mapcar (lambda (x) (format "%s=%s" (car x) (cdr x))) env)
                              process-environment)))
    (with-current-buffer buf 
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "NEO Executing: %s\n\n" (mapconcat #'identity full-command " ")))
        (when show-buffer (display-buffer buf))))

    (let ((proc (make-process
                 :name "neo-subtask"
                 :buffer buf
                 :command full-command
                 :sentinel (lambda (proc _event)
                             (when (eq (process-status proc) 'exit)
                               (let ((status (process-exit-status proc))
                                     (p-buf (process-buffer proc)))
                                 (if (zerop status)
                                     (when on-success (funcall on-success))
                                   (when p-buf (display-buffer p-buf))
                                   (when on-error (funcall on-error)))))))))
      
      ;; If SYNC is requested, wait right here.
      (when sync
        (while (accept-process-output proc))
        ;; Returning the process status or exit code can be useful for sync calls
        (process-exit-status proc))
      
      ;; Return the process object for async use
      proc)))

(cl-defun neo/execute-with-venv (command &key args venv-path env on-success on-error buffer-name (show-buffer t) sync)
  "Execute COMMAND inside a Python virtual environment.
VENV-PATH defaults to .python inside `user-emacs-directory`."
  (let* ((venv (or venv-path (expand-file-name ".python" user-emacs-directory)))
         (bin-dir (expand-file-name "bin" venv))
         (python-exec (expand-file-name "python" bin-dir))
         ;; Standard venv environment variables
         (venv-env `(("VIRTUAL_ENV" . ,venv)
                     ("PATH" . ,(concat bin-dir ":" (getenv "PATH")))))
         ;; Merge with any additional user-provided env vars
         (final-env (append venv-env env)))
    
    ;; Use the python executable from the venv to run the command
    ;; if the command is a .py file, otherwise run the command directly
    (neo/execute command 
                 :args args
		 :sync sync
                 :env final-env 
                 :on-success on-success 
                 :on-error on-error 
                 :buffer-name buffer-name 
                 :show-buffer show-buffer)))
(provide 'neo-processes)
