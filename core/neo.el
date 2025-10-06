(defun neo--require (feature)
  (message "Requiring %s" feature)
  (require feature))

(neo--require 'neo-struct)

(neo--require 'neo-custom)			; customization groups
(neo--require 'neo-utils)			; utility functions
(neo--require 'neo-config)
(neo--require 'neo-packages)

(neo--require 'neo-extensions)
(neo--require 'neo-extensions-fetch)

(provide 'neo)
