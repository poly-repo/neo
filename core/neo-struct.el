(require 'cl-lib)

(cl-defstruct neo
  "Represents the Neo Emacs configuration framework."
  (available-extensions (make-hash-table :test 'equal))
  (installed-extensions (make-hash-table :test 'equal)))

(defvar neo nil
  "The global instance of the Neo Emacs configuration framework.")

(cl-defmethod neo-fetch-extensions ((neo neo))
  "Fetch all available extensions for NEO.
This method populates the `available-extensions' slot of the NEO struct."
  ;; The user will populate this later.
  )

(defun neo-new ()
  "Create a new `neo' instance and fetch extensions."
  (let ((instance (make-neo)))
    (neo-fetch-extensions instance)
    instance))

(provide 'neo-struct)
