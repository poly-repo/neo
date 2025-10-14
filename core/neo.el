(defun neo--require (feature)
  (message "Requiring %s" feature)
  (require feature))

(neo--require 'neo-custom)			; customization groups
(neo--require 'neo-utils)			; utility functions
(neo--require 'neo-config)
(neo--require 'neo-packages)

;(neo--require 'neo-struct)
;(neo--require 'neo-extensions-fetch)

(neo--require 'neo-extensions)

;;; EXECUTE
(neo/maybe-fetch-extensions)
(setq extensions (neo--load-extension-manifests (format "~/.cache/%s/neo-extensions.el" (neo/get-emacs-instance-name))))

(neo--dump-extension-names-and-descriptions extensions)

;;; Actually load the extensions
(neo/load-extensions extensions)

;; (require 'neo-extensions-summary)
;; (neo/extensions-summary-open-buffer (neo--sorted-extensions-by-name extensions))

(require 'neo-packages)
(neo/replay-extension-packages "neo" "questionable-defaults")
(neo/replay-extension-packages "neo" "ui")
(neo/replay-extension-packages "neo" "session")
(neo/replay-extension-packages "neo" "org")
(neo/replay-extension-packages "neo" "terminal")
(neo/replay-extension-packages "neo" "ai")

;; (defun neo-new ()
;;   "Create a new `neo' instance and fetch extensions."
;;   (let ((instance (make-neo)))
;;     (neo-fetch-extensions instance)
;;     instance))

;; ;; Instantiate the global `neo' object.
;; ;; This should be done after all the methods are defined.
;; (setq neo (neo-new))

(provide 'neo)
