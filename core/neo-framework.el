;;; -*- lexical-binding: t -*-

(require 'neo-extensions)
(require 'neo-config)

(cl-defstruct (neo-framework
               (:constructor make-neo-framework (&key available-extensions
                                                     installed-extensions)))
  "A Neo framework instance."
  available-extensions
  installed-extensions)

(defvar neo--framework nil
  "Singleton `neo-framework' instance. Use `neo/framework-instance' to access.")

(defvar neo/after-framework-bootstrap-hook nil
  "Hook run after the Neo framework has finished bootstrapping.")

(defvar neo/framework-bootstrapped-p nil
  "Non-nil after Neo has replayed extension package declarations.")

(defun neo--make-framework-from-cache (&optional instance)
  "Create a `neo-framework' populated from cache for INSTANCE.
If no cache exists, create empty hash tables for the slots."
  (let* ((available (neo--load-extensions-manifest))
         (installed (make-hash-table :test 'equal)))
    (make-neo-framework :available-extensions available
                        :installed-extensions installed)))

;; (defun neo--make-framework-from-cache (&optional instance)
;;   "Create a `neo-framework' populated from cache for INSTANCE.
;; If no cache exists, create empty hash tables for the slots."
;;   (let* ((available (neo--load-extensions-manifest))
;;          (installed (make-hash-table :test 'equal)))
;;     (make-neo-framework :available-extensions available
;;                         :installed-extensions installed)))

(defun neo--framework-instance ()
  "Return the global `neo-framework' singleton, creating it from cache if needed."
  (or neo--framework
      (setq neo--framework (neo--make-framework-from-cache))))

(cl-defgeneric neo/install-extension (framework installation)
  "Add INSTALLATION to the list of installed extensions in FRAMEWORK.")

(cl-defmethod neo/install-extension ((framework neo-framework) installation)
  "Add INSTALLATION to the list of installed extensions in FRAMEWORK."
  (let* ((installed (neo-framework-installed-extensions framework))
         (slug (neo/installation-extension-slug installation))
         (slug-string (neo/extension-slug-to-string slug)))
    (neo/log-info 'core "Adding %s" slug-string)
;    (message "Adding %s" slug-string)
    (puthash slug-string installation installed)))

(defun neo/bootstrap (framework)
  ;; TODO probably need to be moved to neo-framework and renamed maybe 'bootstrap'.
  (setq neo/framework-bootstrapped-p nil)
  (let* ((config-val (neo/get-config "enabled-extensions"))
         (enabled-extensions (if config-val (read config-val) nil))
         (extensions (neo/topo-sort-from-roots (neo-framework-available-extensions framework)
					      enabled-extensions)))
    (neo/install-extensions-from-slugs framework extensions))
  (neo/load-installed-extensions framework)
  (neo/replay-installed-extensions-packages framework))

  
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

(cl-defgeneric neo/load-installed-extensions (framework)
  "Load all installed extensions for FRAMEWORK.")

(cl-defmethod neo/load-installed-extensions ((framework neo-framework))
  "Load all installed extensions for FRAMEWORK."
  (let* ((installed-map (neo-framework-installed-extensions framework))
         (available (neo-framework-available-extensions framework))
         (installed-slugs (let (keys) (maphash (lambda (k _v) (push k keys)) installed-map) keys))
         ;; Sort installed extensions topologically to ensure dependencies are loaded first
         (sorted-slugs (neo/topo-sort-from-roots available installed-slugs '(:on-missing ignore)))
         (sorted-installations '()))
    
    ;; Collect installation objects in sorted order
    (dolist (slug sorted-slugs)
      (when-let* ((inst (gethash slug installed-map)))
        (push inst sorted-installations)))
    (setq sorted-installations (nreverse sorted-installations))
    
    ;; Use the common loader
    (neo/load-extensions sorted-installations available)))

(cl-defgeneric neo/get-extension (framework slug)
  "Load all installed extensions for FRAMEWORK.")

(cl-defmethod neo/get-extension ((framework neo-framework) slug)
  (gethash slug (neo-framework-available-extensions framework)))

(cl-defgeneric neo/replay-installed-extensions-packages (framework)
  "Replay package configurations for all installed extensions in FRAMEWORK.")

(cl-defmethod neo/replay-installed-extensions-packages ((framework neo-framework))
  "Replay package configurations for all installed extensions in FRAMEWORK.

Extensions are visited in the same dependency-respecting topological
order used by `neo/load-installed-extensions', so that a `neo/use-package'
declaration in a dependency extension is always installed/configured
before the declarations of extensions that `:require' it -- `maphash'
order over `installed-extensions' gives no such guarantee.

Unlike the single-extension `neo/replay-extension-packages', this method
has visibility across all installed extensions: `neo--collect-package-sources'
groups every extension's queued declarations by package name first, so
that when two extensions declare the same package, `neo--replay-merged-package'
merges them into a single `use-package' form instead of evaluating two
separate ones (see `neo--merge-use-package-declarations')."
  (let* ((installed-map (neo-framework-installed-extensions framework))
         (available (neo-framework-available-extensions framework))
         (installed-slugs (let (keys) (maphash (lambda (k _v) (push k keys)) installed-map) keys))
         (sorted-slugs (neo/topo-sort-from-roots available installed-slugs '(:on-missing ignore))))
    (setq neo--package-provenance-table (make-hash-table :test 'equal))
    (dolist (entry (neo--collect-package-sources sorted-slugs installed-map neo--enabled-packages))
      (neo--replay-merged-package (car entry) (cdr entry))))
  (setq neo/framework-bootstrapped-p t)
  (run-hooks 'neo/after-framework-bootstrap-hook))

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
  (neo/render-debug-info (neo--framework-instance)))

(provide 'neo-framework)
