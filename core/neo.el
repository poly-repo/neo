(defun neo--require (feature)
  (message "Requiring %s" feature)
  (require feature))

(neo--require 'neo-custom)			; customization groups
(neo--require 'neo-utils)			; utility functions
(neo--require 'neo-config)
(neo--require 'neo-packages)

(neo--require 'neo-struct)

;(neo--require 'neo-extensions)

;; Instantiate the global `neo' object.
;; This should be done after all the methods are defined.
(setq neo (neo-new))

(provide 'neo)
