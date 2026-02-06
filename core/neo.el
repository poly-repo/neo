(setq debug-on-error t)

(setq message-log-max 100000)


;;;###autoload
(defvar neo/use-extensions  t
  "When t we use neo extension mechanism. nil make immediate use of use-package")

(defun neo--require (feature)
  "A wrapper around `require` for FEATURE."
  (require feature))

(neo--require 'neo-utils)
(neo--require 'neo-window)
(neo--require 'neo-custom)			; customization groups
(neo--require 'neo-config)			; configuration db
(neo--require 'neo-packages)			; package management
(neo--require 'neo-logging)
(neo--require 'neo-framework)			; neo infra and core extension management

(neo/set-emacs-source-directories)

(use-package project :ensure t)

(neo/maybe-fetch-extensions)

(setq neo--framework (neo--framework-instance))
(neo/bootstrap neo--framework)

(provide 'neo)
