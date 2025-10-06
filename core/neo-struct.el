(require 'cl-lib)

(cl-defstruct neo
  "Represents the Neo Emacs configuration framework."
  (available-extensions (make-hash-table :test 'equal))
  (installed-extensions (make-hash-table :test 'equal)))

(defvar neo nil
  "The global instance of the Neo Emacs configuration framework.")

(provide 'neo-struct)
