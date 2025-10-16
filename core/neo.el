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

(defvar neo/extensions-loaded-hook nil
  "Hook run after all configured extensions are considered loaded.")

;;; EXECUTE
(neo/maybe-fetch-extensions)
(setq extensions (neo--load-extension-manifests (format "~/.cache/%s/neo-extensions.el" (neo/get-emacs-instance-name))))

(neo--dump-extension-names-and-descriptions extensions)

;;; Actually load the extensions
(defvar neo/my-enabled-extensions
  '("neo:questionable-defaults"
    "neo:ui"
    "neo:session"
    "neo:org"
    "neo:terminal"
    "neo:lsp"
    "neo:ai")
  "Temporary list of extensions to load.")

(setq neo/installed-extensions
      (mapcar (lambda (slug-string)
                (make-neo/installation
                 :extension-slug (neo/make-extension-slug-from-string slug-string)
                 :installed-at (current-time)))
              neo/my-enabled-extensions))

(neo/load-extensions neo/installed-extensions)

;; (require 'neo-extensions-summary)
;; (neo/extensions-summary-open-buffer (neo--sorted-extensions-by-name extensions))

(run-hooks 'neo/extensions-loaded-hook)

;; (defun neo-new ()
;;   "Create a new `neo' instance and fetch extensions."
;;   (let ((instance (make-neo)))
;;     (neo-fetch-extensions instance)
;;     instance))

;; ;; Instantiate the global `neo' object.
;; ;; This should be done after all the methods are defined.
;; (setq neo (neo-new))

(provide 'neo)
