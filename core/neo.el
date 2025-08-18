(defun neo--require (feature)
  (message "Requiring %s" feature)
  (require feature))

(neo--require 'neo-custom)			; customization groups
(neo--require 'neo-utils)			; utility functions, nothing executed here
(neo--require 'neo-config)
(neo--require 'neo-packages)
(neo--require 'neo-extensions)

(provide 'neo)
