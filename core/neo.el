(setq message-log-max 100000)


;;;###autoload
(defvar neo/use-extensions  t
  "When t we use neo extension mechanism. nil make immediate use of use-package")


(defun neo--require (feature)
  "A wrapper around `require` for FEATURE."
  (require feature))

(neo--require 'neo-custom)			; customization groups
(neo--require 'neo-packages)			; package management
(neo--require 'neo-framework)			; neo infra and core extension management

(setq neo--framework (neo--framework-instance))

;; TODO probably need to be moved to neo-framework and renamed maybe 'bootstrap'.
(defun test ()
  "A test function to install and load the 'full-monty' extension set.
This is intended for development and debugging purposes."
  (interactive)
  
  (let ((extensions (neo/topo-sort-from-roots (neo-framework-available-extensions neo--framework)
					      '("neo:full-monty"))))
    (neo/install-extensions-from-slugs neo--framework extensions))
  (neo/load-installed-extensions neo--framework)
  (neo/replay-installed-extensions-packages neo--framework))

(provide 'neo)
