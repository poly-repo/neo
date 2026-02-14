;; -*- lexical-binding: t; -*-

(require 'cl-lib)



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
      (set-process-query-on-exit-flag proc nil)
      
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


(defvar neo--disposable-processes '()
  "List of rx-style patterns for disposable process buffers.")

(defun neo/register-disposable-process (rx-form)
  "Add an rx-style RX-FORM to the disposable process list."
  (cl-pushnew rx-form neo--disposable-processes :test #'equal))

(defun neo--advise-silence-disposable-process (proc)
  "Check if PROC should be silenced based on NEO registry."
  (let ((buf (process-buffer proc)))
    (when (and buf 
               (cl-some (lambda (pattern) 
                          (string-match-p (eval pattern) (buffer-name buf)))
                        neo--disposable-processes))
      (message "ANNOTATING PROCESS %S" proc)
      (set-process-query-on-exit-flag proc nil)
      (with-current-buffer buf
        (setq-local buffer-save-without-query t)
        )))
  proc)

(advice-add 'make-process :filter-return #'neo--advise-silence-disposable-process)

(provide 'neo-processes)
