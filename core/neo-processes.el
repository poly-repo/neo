;; -*- lexical-binding: t; -*-

(require 'cl-lib)

(cl-defun neo/execute (command &key args on-success on-error buffer-name (show-buffer t))
  "Execute COMMAND with ARGS asynchronously.
If SHOW-BUFFER is nil, stay quiet unless an error occurs."
  (let ((buf (get-buffer-create (or buffer-name "*NEO Subprocess*")))
        (full-command (append (list command) args)))
    (with-current-buffer buf 
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "NEO Executing: %s\n\n" (mapconcat #'identity full-command " ")))
        ;; Only display if the user wants it or if we are debugging
        (when show-buffer (display-buffer buf))))
    (make-process
     :name "neo-subtask"
     :buffer buf
     :command full-command
     :sentinel (lambda (proc _event)
                 (when (eq (process-status proc) 'exit)
                   (let ((status (process-exit-status proc))
                         (p-buf (process-buffer proc)))
                     (if (zerop status)
                         (progn
                           (when on-success (funcall on-success)))
                       ;; On Error: Always show the buffer regardless of show-buffer setting
                       (when p-buf (pop-to-buffer p-buf))
                       (when on-error (funcall on-error)))))))))

(provide 'neo-processes)
