(require 'cl-lib)

(defvar neo/--emacs-instance-name-cache nil
  "Cached result of `neo/get-emacs-instance-name`.")

(defun neo/get-emacs-instance-name ()
  "Return the Emacs instance name from `--name`, or fallback to `emacs`.

Reads from /proc/self/cmdline if needed (Linux-only)."
  (or neo/--emacs-instance-name-cache
      (setq neo/--emacs-instance-name-cache
            (or
             ;; Try $EMACS_NAME for compatibility or scripting
             (getenv "EMACS_NAME")
             ;; Try /proc/self/cmdline
             (when (and (eq system-type 'gnu/linux)
                        (file-readable-p "/proc/self/cmdline"))
               (with-temp-buffer
                 (insert-file-contents-literally "/proc/self/cmdline")
                 (let* ((args (split-string (buffer-string) "\0" t))
                        (index (cl-position "--name" args :test #'string=)))
                   (when index
                     (nth (1+ index) args)))))
             ;; Fallback
             "neo"))))

(provide 'early-init-utils)
