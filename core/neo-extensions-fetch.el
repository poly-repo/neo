;;; neo-extensions-fetch.el --- Fetch extensions manifest -*- lexical-binding: t; -*-

;; Author: you
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; This file provides the functionality to fetch the Neo Emacs extensions manifest.
;; It uses the `neo-asset` library to handle downloading and caching.

;;; Code:

(require 'neo-asset)
(require 'neo-struct)

(defgroup neo nil
  "Neo Emacs configuration framework."
  :group 'tools)

(defcustom neo/extensions-manifest-url
  "https://raw.githubusercontent.com/neo-emacs/extensions/main/dist/neo-extensions.el"
  "URL for the Neo extensions manifest file.
This file is expected to be an Emacs Lisp file that sets the
variable `neo-extensions-list' to a list of extension plists."
  :type 'string
  :group 'neo)

(defun neo-fetch-extensions (neo)
  "Fetch and load available extensions into NEO instance.
This function downloads the manifest specified by
`neo/extensions-manifest-url', loads it, and populates the
`available-extensions' slot of the `neo' struct instance."
  (let* ((asset (neo/make-extensions-config
                 :name "neo-extensions"
                 :remote neo/extensions-manifest-url
                 :error-on-missing nil)) ; Don't error if offline
         (manifest-file (neo/ensure asset)))
    (when manifest-file
      (condition-case err
          (with-temp-buffer
            ;; Load into temp buffer to avoid polluting the global namespace
            ;; with anything other than the expected variable.
            (let ((load-prefer-newer t)
                  (neo-extensions-list nil))
              (insert-file-contents manifest-file)
              (eval-buffer)
              (if (listp neo-extensions-list)
                  (progn
                    (clrhash (neo-available-extensions neo)) ; Clear old data
                    (dolist (ext neo-extensions-list)
                      (let ((name (plist-get ext :name)))
                        (when (and name (symbolp name))
                          (puthash (symbol-name name) ext (neo-available-extensions neo))))))
                (message "[neo] extensions manifest did not define `neo-extensions-list` as a list"))))
        (error (message "[neo] failed to load extensions manifest %s: %s"
                        manifest-file (error-message-string err)))))))

(provide 'neo-extensions-fetch)

;;; neo-extensions-fetch.el ends here
