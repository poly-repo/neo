;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'cl-generic)
(require 'neo-extensions)

(cl-defstruct neo-framework
  "Represents the Neo Emacs configuration framework."
  (available-extensions (make-hash-table :test 'equal))
  (installed-extensions (make-hash-table :test 'equal)))

(defvar neo nil
  "The global instance of the Neo Emacs configuration framework.")

(defun neo/make-from-cache ()
  "Create a `neo` instance, populating available extensions from cache."
  (let* ((instance (neo/get-emacs-instance-name))
         (extensions-file (expand-file-name (format ".cache/%s/neo-extensions.el" instance) "~"))
         (available (if (file-exists-p extensions-file)
                        (neo--load-extension-manifests extensions-file)
                      (make-hash-table :test 'equal))))
    (make-neo-framework :available-extensions available)))

(defun neo/get-instance ()
  "Return the global `neo` instance, initializing it from cache if needed."
  (or neo
      (setq neo (neo/make-from-cache))))

(cl-defgeneric neo/install-extension (framework installation)
  "Add INSTALLATION to the list of installed extensions in FRAMEWORK.")

(cl-defmethod neo/install-extension ((framework neo-framework) installation)
  "Add INSTALLATION to the list of installed extensions in FRAMEWORK."
  (let* ((installed (neo-framework-installed-extensions framework))
         (slug (neo/installation-extension-slug installation))
         (slug-string (neo/extension-slug-to-string slug)))
    (puthash slug-string installation installed)))

;; This is a temporary function that will be made unnecessary when the UI can
;; select extensions to be installed and we're able to persist and retrieve that
;; selection.
(cl-defgeneric neo/install-extensions-from-slugs (framework slugs)
  "Install extensions from a list of SLUGS into FRAMEWORK.")

(cl-defmethod neo/install-extensions-from-slugs ((framework neo-framework) slugs)
  "Install extensions from a list of SLUGS into FRAMEWORK.
SLUGS can be a list of `neo/extension-slug` objects or strings
in 'publisher:name' format."
  (dolist (slug-item slugs)
    (let* ((slug (if (stringp slug-item)
                     (neo/make-extension-slug-from-string slug-item)
                   slug-item))
           (installation (make-neo/installation
                          :extension-slug slug
                          :installed-at (current-time))))
      (neo/install-extension framework installation))))

(cl-defgeneric neo/render-debug-info (framework)
  "Render debug info for FRAMEWORK into a buffer.")

(cl-defmethod neo/render-debug-info ((framework neo-framework))
  "Render debug info for FRAMEWORK into a buffer."
  (with-current-buffer (get-buffer-create "*neo messages*")
    (erase-buffer)
    (insert (format "--- Neo Debug Information ---\n\n"))
    (insert "Global `neo` instance (summary):\n")
    (insert "\n\nAvailable Extensions ("
            (prin1-to-string (hash-table-count (neo-framework-available-extensions framework)))
            "):\n")
    (maphash (lambda (k v)
               (insert (format "- %s\n" k)))
             (neo-framework-available-extensions framework))
    (insert "\nInstalled Extensions ("
            (prin1-to-string (hash-table-count (neo-framework-installed-extensions framework)))
            "):\n")
    (maphash (lambda (k v)
               (insert (format "- %s\n" k)))
             (neo-framework-installed-extensions framework))
    (pop-to-buffer (current-buffer))))

(defun neo/debug-info ()
  "Display debug information about the `neo` instance in a new buffer."
  (interactive)
  (neo/render-debug-info (neo/get-instance)))

(provide 'neo-struct)
