;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'neo-extensions)

(cl-defstruct neo-framework
  "Represents the Neo Emacs configuration framework."
  (available-extensions (make-hash-table :test 'equal))
  (installed-extensions (make-hash-table :test 'equal)))

(defvar neo nil
  "The global instance of the Neo Emacs configuration framework.")

(defun neo/make-from-cache ()
  "Create a `neo` instance, populating available extensions from cache."
  (let* ((instance (neo/get-emacs-instance-name))
         (extensions-file (expand-file-name (format ".cache/%s/neo-extensions.el" instance) "~"))
         (available (if (file-exists-p extensions-file)
                        (neo--load-extension-manifests extensions-file)
                      (make-hash-table :test 'equal))))
    (make-neo-framework :available-extensions available)))

(defun neo/get-instance ()
  "Return the global `neo` instance, initializing it from cache if needed."
  (or neo
      (setq neo (neo/make-from-cache))))

(defun neo/debug-info ()
  "Display debug information about the `neo` instance in a new buffer."
  (interactive)
  (let ((instance (neo/get-instance)))
    (with-current-buffer (get-buffer-create "*neo messages*")
      (erase-buffer)
      (insert (format "--- Neo Debug Information ---\n\n"))
      (insert "Global `neo` instance (summary):\n")
      (insert "\n\nAvailable Extensions ("
              (prin1-to-string (hash-table-count (neo-framework-available-extensions instance)))
              "):\n")
      (maphash (lambda (k v)
                 (insert (format "- %s\n" k)))
               (neo-framework-available-extensions instance))
      (insert "\nInstalled Extensions ("
              (prin1-to-string (hash-table-count (neo-framework-installed-extensions instance)))
              "):\n")
      (maphash (lambda (k v)
                 (insert (format "- %s\n" k)))
               (neo-framework-installed-extensions instance))
      (pop-to-buffer (current-buffer)))))

(provide 'neo-struct)
