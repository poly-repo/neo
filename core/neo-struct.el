(require 'cl-lib)
(require 'neo-extensions-fetch)

(cl-defstruct neo
  "Represents the Neo Emacs configuration framework."
  (available-extensions (make-hash-table :test 'equal))
  (installed-extensions (make-hash-table :test 'equal)))

(defun neo-new ()
  "Create a new `neo' instance and fetch extensions."
  (let ((instance (make-neo)))
    (neo-fetch-extensions instance)
    instance))

(defvar neo nil
  "The global instance of the Neo Emacs configuration framework.")

(provide 'neo-struct)
