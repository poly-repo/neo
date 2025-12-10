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

(neo/bootstrap neo--framework)

(provide 'neo)
