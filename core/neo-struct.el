;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'neo-extensions)

(cl-defstruct neo
  "Represents the Neo Emacs configuration framework."
  (available-extensions (make-hash-table :test 'equal))
  (installed-extensions (make-hash-table :test 'equal)))

(defvar neo nil
  "The global instance of the Neo Emacs configuration framework.")

(defun neo/make-from-cache ()
  "Create a `neo` instance, populating available extensions from cache."
  (let* ((instance (neo/get-emacs-instance-name))
         (extensions-file (expand-file-name (format ".cache/%s/neo-extensions.el" instance) "~"))
         (available (make-hash-table :test 'equal)))
    (when (file-exists-p extensions-file)
      (let ((neo--extensions (make-hash-table :test 'equal)))
        (load extensions-file nil 'nomessage 'nosuffix)
        (setq available neo--extensions)))
    (make-neo :available-extensions available)))

(provide 'neo-struct)
