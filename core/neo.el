(defun neo--require (feature)
  (message "Requiring %s" feature)
  (require feature))

(neo--require 'cl-lib)

(cl-defstruct neo
  "Represents the Neo Emacs configuration framework."
  (available-extensions (make-hash-table :test 'equal))
  (installed-extensions (make-hash-table :test 'equal)))

(defvar neo (make-neo)
  "The global instance of the Neo Emacs configuration framework.")

(neo--require 'neo-custom)			; customization groups
(neo--require 'neo-utils)			; utility functions
(neo--require 'neo-config)
(neo--require 'neo-packages)

(neo--require 'neo-extensions)

(provide 'neo)
