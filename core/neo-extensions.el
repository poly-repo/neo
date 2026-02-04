;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'cl-generic)
(require 'subr-x)

(require 'neo-extensions-digest)

(defgroup neo-extensions nil
  "Settings for Neo Extensions system."
  :group 'neo
  :prefix "Neo/extensions-")

(cl-defstruct neo/repository
  "Represents the source location of an extension repository."
  type
  url
  path)

(cl-defstruct (neo/extension
               (:copier nil))
  "Represents a single Neo extension."
  name
  title
  publisher
  emblem
  description
  categories
  keywords
  provides
  requires
  depends-on
  repository
  ;; internal runtime state
  summary-overlay  ;; used in summary view only
  emblem-img       ;; derived from emblem
  emblem-hilighted-img ;; derived from emblem applying a solid
  ;; background matching highlight. Changes with theme setting, need to
  ;; register a hook for that.
  )

(cl-defstruct (neo/extension-slug
               (:print-function
                (lambda (obj stream depth)
                  (declare (ignore depth))
                  (princ (neo/extension-slug-to-string obj) stream))))
  "A unique identifier for an extension, combining publisher and name."
  (publisher "" :type string)
  (name "" :type string))

(defun neo/extension-slug-to-string (slug)
  "Convert a `neo/extension-slug` object to a 'publisher:name' string."
  (format "%s:%s"
          (neo/extension-slug-publisher slug)
          (neo/extension-slug-name slug)))


(defun neo/extension-feature-symbol (slug-or-string)
  (let* ((slug-string (if (neo/extension-slug-p slug-or-string) (neo/extension-slug-to-string slug-or-string) slug-or-string)))
    (cl-destructuring-bind (publisher name)
	(split-string slug-string ":" t)
      (intern (format "neo-extension-%s-%s" publisher name)))))

(defun neo/make-extension-slug-from-string (slug-string)
  "Create a `neo/extension-slug` from a string like 'publisher:name'."
  (let* ((parts (split-string slug-string ":" t)))
    (unless (= (length parts) 2)
      (error "Invalid extension slug format: %s. Expected 'publisher:name'." slug-string))
    (make-neo/extension-slug :publisher (car parts) :name (cadr parts))))

(defun neo/extensionp (feature)
  (featurep (neo/extension-feature-symbol feature)))

(cl-defstruct neo/installation
  "Represents an installed extension."
  extension-slug   ;; `neo/extension-slug` referring to a `neo/extension`
  recommended-by   ;; list of `neo/extension-slug` objects
  suggested-by     ;; list of `neo/extension-slug` objects
  installed-at)    ;; timestamp

(cl-defstruct neo/orphaned-extension
  "Represents an extension that is installed but no longer available."
  slug)

(defvar neo/installed-extensions nil
  "A list of `neo/installation` objects, defining which extensions are installed and should be loaded.")

(cl-defgeneric neo/render (object)
  "Render OBJECT at point and return a point marker.")

(cl-defmethod neo/render-details ((ext neo/extension))
  "Render EXT in the current buffer. Return (start . end) position."
  (when-let ((emblem (neo/extension-emblem ext)))
    (when (stringp emblem)
      (insert-image (create-image emblem 'png t))
      (insert " ")))

  ;; Title
  (insert (propertize (or (neo/extension-title ext) "Unnamed")
                      'face '(:weight bold :height 1.5)))
  (insert "\n\n")
  )

(defvar neo/info-icon-map
  (let ((map (make-sparse-keymap)))
    ;; prevent clicks from going through to surrounding buttons or rows
    (define-key map [mouse-1] #'ignore)
    (define-key map [mouse-2] #'ignore)
    (define-key map [mouse-3] #'ignore)
    map)
  "Keymap for info icons to prevent buffer-switching clicks.")

(defun neo/show-posframe-info (info)
  "Show INFO (list of (label . value)) in a posframe popup."
  (let* ((label-face '(:weight bold))
         (value-face '(:foreground "#6cf"))
         (text (mapconcat (lambda (pair)
                            (concat
                             (propertize (format "%-10s" (car pair)) 'face label-face)
                             (propertize (cdr pair) 'face value-face)))
                          info "\n")))
    (when (featurep 'posframe)
      (posframe-show
       "*neo-info*"
       :string text
       :position (point)
       :timeout 5
       :background-color (face-background 'tooltip nil t)
       :internal-border-width 10
       :internal-border-color "#888"))))

(defun neo/make-hover-callback (info)
  "Return a help-echo callback that displays INFO on hover."
  (lambda (_win _obj _pos)
    (neo/show-posframe-info info)
    ;; Returning a string here still lets the echo area show text too if desired
    nil))

(cl-defmethod neo/render ((ext neo/extension))
  "Render EXT in the current buffer. Return (start . end) position."
  (let ((start (point)))
    ;; Insert image using overlay
    (let* ((emblem (neo/extension-emblem ext))
	   (img (if (stringp emblem)
		    (create-image emblem 'png t :data-p t)
		  (create-image (expand-file-name "assets/default-emblem64.png" user-emacs-directory) 'png))))
      ;; Insert a space and record its bounds *after* insertion
      (insert " ")
      (let ((ov (make-overlay (1- (point)) (point))))
        (overlay-put ov 'display img)
        (overlay-put ov 'neo-image t)
        ;; Store it in the extension struct
        (setf (neo/extension-summary-overlay ext) ov)))
    
    (insert " ") ;; optional visual spacing

    (insert (propertize (or (neo/extension-title ext) "Unnamed")
                        'face '(:weight bold :height 1.2)))
    (insert "\n\n")

    ;; Description
    (when-let ((desc (neo/extension-description ext)))
      (insert (propertize desc 'face '(:slant italic :height 0.95)))
      (insert "\n\n"))

    (neo/insert-thin-divider)
    
    ;; Final range
    (let ((end (point)))
      (put-text-property start end 'neo-extension ext)
      (cons start end))))

(cl-defmethod neo/render ((ext neo/orphaned-extension))
  "Render an orphaned extension, indicating it's no longer available."
  (let ((start (point)))
    (insert (propertize (format "Orphaned Extension: %s"
                                (neo/extension-slug-to-string (neo/orphaned-extension-slug ext)))
                        'face '(:foreground "orange red" :weight bold :height 1.2)))
    (insert "\n\n")
    (insert (propertize "This extension is installed but no longer appears in the list of available extensions. It might have been renamed or removed by its publisher."
                        'face '(:slant italic :height 0.95)))
    (insert "\n\n")
    (neo/insert-thin-divider)
    (let ((end (point)))
      (put-text-property start end 'neo-extension ext)
      (cons start end))))

(defface neo/divider-face
  '((t :overline unspecified
       :foreground unspecified
       :background unspecified))
  "Face for UI dividers.")

(defun neo/update-divider-face ()
  "Update `neo/divider-face` colors to match the current theme."
  (let ((fg (face-foreground 'default nil t))
        (bg (face-background 'default nil t)))
    (set-face-attribute 'neo/divider-face nil
                        :overline fg
                        :foreground bg
                        :background bg)))

(neo/update-divider-face)
(add-hook 'neo/after-theme-load-hook #'neo/update-divider-face)

(defun neo/insert-thin-divider (&optional color)
  "Insert a thin horizontal divider using underline, aligned to window width.
If COLOR is nil, use the theme's default foreground color."
  (let* ((line-color (or color (face-foreground 'default nil t)))
         (bg-color   (face-background 'default nil t)))
					;    (insert "\n")
    (insert
     (propertize " "
                 'display '(space :align-to right)
                 'face 'neo/divider-face))
    (insert "\n")))

(defun neo/show-extension-info (info-list)
  "Show INFO-LIST (alist of (label . value)) in a posframe or tooltip.
Labels are bolded, values are colored."
  (let* ((label-face '(:weight bold))
         (value-face '(:foreground "#6cf"))
         (formatted-lines
          (mapcar (lambda (pair)
                    (concat
                     (propertize (format "%-10s" (car pair)) 'face label-face)
                     (propertize (cdr pair) 'face value-face)))
                  info-list))
         (info-text (string-join formatted-lines "\n")))
    (if (featurep 'posframe)
        (posframe-show "*neo-extension-info*"
                       :string info-text
                       :background-color (face-background 'tooltip nil t)
                       :position (point)
                       :timeout 5
                       :internal-border-width 10
                       :internal-border-color "#888")
      (tooltip-show info-text))))

(defun neo/make-info-hover-handler (info-list)
  "Return a hover handler for INFO-LIST usable as `help-echo'."
  (lambda (_win _obj _pos)
    (neo/show-extension-info info-list)))

(cl-defmethod neo/render ((repo neo/repository))
  "Insert NE0/REPOSITORY REPO details at point. Return point marker."
  (when (neo/repository-type repo)
    (insert (format "%-12s: %s\n" "Type" (neo/repository-type repo))))
  (when (neo/repository-url repo)
    (insert (format "%-12s: %s\n" "URL"  (neo/repository-url repo))))
  (when (neo/repository-path repo)
    (insert (format "%-12s: %s\n" "Path" (neo/repository-path repo))))
  (point-marker))

(cl-defmethod neo/render ((repo neo/repository))
  "Render repository details inline."
  (dolist (key '(type url path))
    (when-let ((val (slot-value repo key)))
      (insert (format "%-12s: %s\n" (capitalize (symbol-name key)) val)))))

(defvar neo--extensions (make-hash-table :test #'equal)
  "Hash table mapping publisher:name to `neo/extension` instances.")

(defun neo--normalize-name (name)
  "Normalize NAME by downcasing and replacing spaces with dashes."
  (replace-regexp-in-string
   " " "-"
   (downcase name)))

(defun neo--extension-slug (ext)
  "Create a `neo/extension-slug` from a `neo/extension` object EXT."
  (make-neo/extension-slug :publisher (neo/extension-publisher ext)
                           :name (neo--normalize-name (neo/extension-name ext))))

;;; This is for debugging
(defun neo--dump-extension-names-and-descriptions (extensions)
  "Display names and descriptions from `neo--extensions` in a temporary buffer."
  (interactive)
  (let ((buf (generate-new-buffer "*Neo Extensions Summary*")))
    (with-current-buffer buf
      (insert ";; Neo Extensions - Names and Descriptions\n\n")
      (maphash
       (lambda (_key ext)
         (insert (format "• %s\n  %s\n\n"
                         (or (neo/extension-name ext) "Unnamed")
                         (or (neo/extension-description ext) "No description.")))
	 (insert (format "  Requires: %s\n"
                         (mapconcat #'identity (neo/extension-requires ext) ", ")))
	 (insert "\n\n"))
       extensions))
    (pop-to-buffer buf)))


(defmacro neo/extension (&rest args)
  "Register a new Neo extension and store it in `neo--extensions`.

Args:
- :name NAME (string)
- :publisher PUBLISHER (string)
- :description DESCRIPTION (string)
- :categories (LIST of symbols)
- :keywords (LIST of symbols)
- :requires (LIST of symbols or a single symbol)
- :repository (plist with :type, :url, :path)

This macro relies on two dynamic variables being bound:

- neo--extensions-emblem-path: a path to an emblem file, that might or
  might not exist. It never exist for aggregate digests, it might exist
  for individual manifests.
- neo--extensions: a hash table where we store extensions
"
  (let* ((name (plist-get args :name))
	 (title (plist-get args :title))
	 (normalized-name (neo--normalize-name name))
         (publisher (plist-get args :publisher))
         (desc (plist-get args :description))
         (cats (plist-get args :categories))
         (tags (plist-get args :keywords))
;;	 (emblem (or (plist-get args :emblem) neo--extensions-emblem-path))
	 (emblem (or (plist-get args :emblem) (neo--read-binary-string-safe neo--extensions-emblem-path)))
	 (requires (plist-get args :requires))
	 (provides (plist-get args :provides))
	 (depends-on (plist-get args :depends-on))
	 (require-list (if (listp requires) requires (list requires)))
	 (provide-list (if (listp provides) provides (list provides)))
	 (depend-list (if (listp depends-on) depends-on (list depends-on)))
	 (repo-raw (plist-get args :repository))
	 (repo (make-neo/repository
		:type (plist-get repo-raw :type)
		:url (plist-get repo-raw :url)
		:path (plist-get repo-raw :path)))
	 (extension (make-neo/extension
		     :name name
		     :title title
		     :publisher publisher
		     :description desc
		     :emblem emblem
		     :categories cats
		     :keywords tags
		     :requires require-list
		     :provides provide-list
		     :depends-on depend-list
		     :repository repo
		     :summary-overlay nil))
	 (slug (neo--extension-slug extension)))
    `(puthash ,(neo/extension-slug-to-string slug) ,extension neo--extensions)))

(defun neo--development-emacs-directory ()
  "Return non-nil if `user-emacs-directory' matches
$HOME/.local/share/wtrees/<anything>/devex/editors/emacs/."
  (let* ((home (file-name-as-directory (expand-file-name "~")))
	 (pattern (rx-to-string
		   `(seq
		     ,(regexp-quote home)
		     ".local/share/wtrees/"
		     (+ (not (any "/"))) ; arbitrary single directory
		     "/devex/editors/emacs/"))))
    (string-match-p pattern user-emacs-directory)))

(defun neo--extensions-base-dir ()
  "Return the base directory containing extension sources.
In development mode, this includes the extra 'extensions' subdirectory.
In production (installed) mode, it is flattened."
  (if (neo--development-emacs-directory)
      (expand-file-name "extensions/extensions/" user-emacs-directory)
    (neo/cache-file-path "extensions/current/extensions/")))

(defun neo--load-extensions-manifest ()
  "Load extension manifests from EXTENSIONS-SUMMARY-FILE.
This populates a temporary `neo--extensions` and returns it."
  (let ((neo--extensions (make-hash-table :test 'equal)))
    (if (and (neo--development-emacs-directory)
	     (or (neo/load-file (expand-file-name "extensions/current/extensions.el" user-emacs-directory) t)
		 (neo/load-file (expand-file-name "extensions/build/extensions.el" user-emacs-directory) t)))
	neo--extensions
      (clrhash neo--extensions)
      (neo/load-cached-file "extensions/current/extensions.el" t)
      neo--extensions)))

(defun neo--sorted-extensions-by-name (extensions)
  "Return a list of `neo/extension` values sorted by name."
  (let (extension-list)
    (maphash (lambda (_k v) (push v extension-list)) extensions)
    (sort extension-list
          (lambda (a b)
            (string< (neo/extension-name a)
                     (neo/extension-name b))))))

(defun neo--normalize-requires (reqs)
  "Normalize REQS (nil, string, or list) to a list of strings."
  (cond
   ((null reqs) nil)
   ((stringp reqs) (list reqs))
   ((listp reqs) reqs)
   (t (error "neo--normalize-requires: unexpected requires value: %S" reqs))))

(defun neo--collect-reachable (ht roots on-missing)
  "Return a list of HT keys reachable from ROOTS following `requires`.
HT is key->struct hash table. ROOTS is a list of key strings.
ON-MISSING controls behavior when a required key is not in HT:
'error, 'warn, or 'ignore.
This returns the list of reachable keys (order unspecified)."
  (let ((seen (make-hash-table :test 'equal))
        (stack (copy-sequence roots))
        reachable)
    (while stack
      (let ((k (pop stack)))
        (unless (gethash k seen)
          (puthash k t seen)
          (if (gethash k ht)
              (progn
                ;; push its requirements onto the stack
                (let ((reqs (neo--normalize-requires
                             (neo/extension-requires (gethash k ht)))))
                  (dolist (r reqs)
                    (unless (gethash r seen)
                      (if (gethash r ht)
                          (push r stack)
                        (pcase on-missing
                          ('error (neo/log-error 'core "neo: %s requires missing %s" k r))
                          ('warn  (neo/log-info 'core "neo: warning: %s requires missing %s (ignored)" k r))
                          ('ignore nil)
                          (_ (neo/log-error 'core "neo--collect-reachable: unknown :on-missing %S" on-missing))))))))
            ;; k itself absent from ht — treat according to on-missing:
            (pcase on-missing
              ('error (neo/log-error 'core "neo: starting node %s not present in table" k))
              ('warn  (neo/log-info 'core "neo: warning: starting node %s not present (ignored)" k))
              ('ignore nil)
              (_ (neo/log-error 'core "neo--collect-reachable: unknown :on-missing %S" on-missing)))))))
    ;; collect keys from 'seen' that are present in ht (if absent we either errored or ignored)
    (mapcar #'car
            (cl-remove-if-not
             (lambda (kv) (gethash (car kv) ht))
             (let (pairs)
               (maphash (lambda (kk vv) (push (cons kk vv) pairs)) seen)
               pairs)))))

;;; Main function
(defun neo/topo-sort-from-roots (ht roots &optional plist)
  "Topologically sort nodes of HT that are reachable from ROOTS.

HT is a hash-table mapping key-string -> (neo/extension ...).
ROOTS is a list of key strings (starting points).  The algorithm
collects the transitive closure of ROOTS following `requires` and
then returns a topological ordering of that induced subgraph.

PLIST options:
:on-missing  one of 'error (default), 'warn, or 'ignore
:on-cycle    one of 'error (default), 'warn, or 'ignore
:return      one of 'keys (default) or 'structs

Return: list of keys or structs in topological order (dependencies
						      appear before dependents).  If a cycle is detected, behavior depends
on :on-cycle (see above)."
  (let* ((on-missing (or (plist-get plist :on-missing) 'error))
         (on-cycle   (or (plist-get plist :on-cycle) 'error))
         (ret-kind   (or (plist-get plist :return) 'keys))
         ;; compute reachable set
         (reachable (neo--collect-reachable ht roots on-missing))
         ;; create maps for indegree and successors only for reachable nodes
         (indegree (make-hash-table :test 'equal))
         (succs (make-hash-table :test 'equal)))
    ;; initialize
    (dolist (k reachable)
      (puthash k 0 indegree)
      (puthash k nil succs))
    ;; build edges restricted to reachable nodes
    (dolist (k reachable)
      (let* ((ext (gethash k ht))
	     ;; TODO normalize-requires is not a precise name any more as we use it for depends-on as well
	     ;; maybe normalize-dependencies
             (reqs (neo--normalize-requires (and ext (neo/extension-requires ext))))
	     (depends-on (neo--normalize-requires (and ext (neo/extension-depends-on ext)))))
	;; NOTE: here we can simply concatenate reqs and depends
	;; on. neo--collect-reachable has already inserted the
	;; transitive closure of what is :required (and errored out if
	;; something is missing). Now we just go over the union of
	;; required and depend on and add dependency edges only if
	;; what is in the list is actually present, which is the
	;; behavior we want from :depend-on
        (dolist (r (append reqs depends-on))
          ;; r might not be in reachable (it will be if present in ht and was discovered),
          ;; if r is missing we already handled it in the collection phase per on-missing
	  ;; but we're ok w/ :depend-on, as explained earlier
          (when (and (gethash r ht) (gethash r indegree)) ; r present and within reachable set
            (puthash r (cons k (gethash r succs)) succs)
            (puthash k (1+ (gethash k indegree)) indegree)))))
    ;; Kahn's algorithm on reachable subgraph
    (let ((queue (cl-remove-if-not (lambda (n) (= 0 (gethash n indegree))) reachable))
          (result-keys '()))
      (while queue
        (let ((n (pop queue)))
          (push n result-keys)
          (dolist (m (gethash n succs))
            (puthash m (1- (gethash m indegree)) indegree)
            (when (= 0 (gethash m indegree))
              (push m queue)))))
      (setq result-keys (nreverse result-keys))
      ;; detect cycles (only within reachable set)
      (if (/= (length result-keys) (length reachable))
          (let* ((remaining (cl-remove-if (lambda (k) (member k result-keys)) reachable)))
            (pcase on-cycle
              ('error (neo/log-error 'core "neo/topo-sort-from-roots: cycle detected among: %S" remaining))
              ('warn  (neo/log-info 'core "neo/topo-sort-from-roots: cycle detected among: %S (returning partial order)" remaining)
                      (if (eq ret-kind 'structs)
                          (mapcar (lambda (k) (gethash k ht)) result-keys)
                        result-keys))
              ('ignore (if (eq ret-kind 'structs)
                           (mapcar (lambda (k) (gethash k ht)) result-keys)
                         result-keys))
              (_ (neo/log-error "neo/topo-sort-from-roots: unknown :on-cycle %S" on-cycle))))
        ;; success: return requested kind
        (if (eq ret-kind 'structs)
            (mapcar (lambda (k) (gethash k ht)) result-keys)
          result-keys)))))

;; TODO here we cheat and relay on the fact that for testing we have
;; the user-emacs-directory pointing into the repo. But this means we
;; have two levels of 'extensions'
(defun neo--load-extension (ext)
  "Load the extension file for EXT.

Missing files are ignored.
If the file exists but errors during load, the error is signaled.

Returns non-nil on successful load, nil if file does not exist."
  (let* ((publisher (neo/extension-publisher ext))
         (name      (neo--normalize-name (neo/extension-name ext)))
         (base      (neo--extensions-base-dir))
         (file-dir  (expand-file-name (format "%s/%s" publisher name) base))
         (file      (expand-file-name (format "neo-%s.el" name) file-dir)))
    (if (not (file-exists-p file))
	nil
      (let ((load-path (cons file-dir load-path)))
        (load file nil 'nomessage 'nosuffix)
					;        (neo/log-info "[neo] Loaded %s" file)
        t))))

(require 'cl-macs) ; for cl-macrolet


(defun neo/load-with-dummy-use-package (file dummy-use-package)
  "Load FILE, overriding `neo/use-package' to just print the first argument.
The original `neo/use-package' is restored afterwards."
  (let ((orig (symbol-function 'neo/use-package)))
    (unwind-protect
        (progn
          ;; Define a dummy macro globally
          (defmacro neo/use-package (name &rest _args)
            `(funcall ,dummy-use-package ',name))
          ;; Load the file
          (load file nil 'nomessage 'nosuffix))
      ;; Restore original macro
      (fset 'neo/use-package orig))))


(defun neo/refresh-package-archives ()
  (unless (and (boundp 'package-archive-contents)
               package-archive-contents)
    (let ((package-user-dir (neo/cache-file-path "elpa-packages")))
      (package-refresh-contents))))

(defun neo/find-package-desc (pkg-name)
  "Return the `package-desc` object for PKG-NAME, or nil if not found."
  (cdr (assoc pkg-name package-archive-contents)))

(defun neo--get-extension-info (ext)
  "Return an alist of (package-name . package-desc) for all packages used by EXT."
  (neo/refresh-package-archives)
  (let* ((publisher (neo/extension-publisher ext))
         (name      (neo--normalize-name (neo/extension-name ext)))
         (base      (neo--extensions-base-dir))
         (file-dir  (expand-file-name (format "%s/%s" publisher name) base))
         (file      (expand-file-name (format "neo-%s.el" name) file-dir)))
    (when (file-exists-p file)
      (let ((load-path (cons file-dir load-path))
            (used-packages '()))
        ;; load the extension, collecting package-name . package-desc alist
        (neo/load-with-dummy-use-package
         file
         (lambda (pkg)
           (push (cons (symbol-name pkg)
                       (neo/find-package-desc pkg))
                 used-packages)))
        ;; reverse so order matches original
        (reverse used-packages)))))


(defun neo/load-extensions (installed-extensions &optional available-extensions)
  "Load extensions specified in INSTALLED-EXTENSIONS.
This function iterates through the INSTALLED-EXTENSIONS list, which is a list
of `neo/installation` objects. It looks up each extension by its slug in the
AVAILABLE-EXTENSIONS hash table (defaulting to `neo--extensions`), and loads it if found."
  (let ((available (or available-extensions neo--extensions)))
    (dolist (installation installed-extensions)
      (let* ((slug (neo/installation-extension-slug installation))
             (slug-string (neo/extension-slug-to-string slug))
             (extension (gethash slug-string available)))
        (if extension
            (progn
              (neo/log-info 'core "Loading extension %s" slug-string)
              (if (neo--load-extension extension)
                  (progn
                    (neo/log-info 'core "  ✔️ Loaded '%s'" (neo/extension-title extension))
                    (provide (neo/extension-feature-symbol slug)))
                (neo/log-warn 'core "  ❌ Extension %s file not found or failed to load" slug-string)))
          (neo/log-warn 'core "  ❌ Extension %s not found in registry" slug-string))))))

;; TODO only fetch if older than X hours unless FORCE is used.
(defun neo/fetch-extensions ()
  "Download and cache the latest neo-extensions.el manifest and repository content.
Manifest goes to ~/.cache/<instance>/extensions/<SHA>/extensions.el (symlinked to current).
Repository content goes to ~/.cache/<instance>/extensions/<SHA>/extensions/ (if not in dev mode)."
  (unless (neo--development-emacs-directory)
    (let* ((base-url "https://github.com/poly-repo/neo-extensions/releases/download/latest/")
           (filename "neo-extensions.el")
           (sha-filename "neo-extensions.sha256")
           (instance (neo/get-emacs-instance-name))
           (cache-dir (expand-file-name (format "~/.cache/%s/" instance)))
           (extensions-dir (expand-file-name "extensions/" cache-dir))
           (temp-sha (make-temp-file "neo-sha" nil ".sha256"))
           (sha-url (concat base-url sha-filename))
           (file-url (concat base-url filename))
           (manifest-updated nil))
      
      (unless (file-directory-p extensions-dir)
        (make-directory extensions-dir t))

      (url-copy-file sha-url temp-sha t)
      
      (let* ((sha-content (with-temp-buffer
                            (insert-file-contents temp-sha)
                            (string-trim (buffer-string))))
             ;; Take first word as SHA
             (sha (car (split-string sha-content)))
             (sha-dir (expand-file-name sha extensions-dir))
             (target-file (expand-file-name "extensions.el" sha-dir))
             (current-link (expand-file-name "current" extensions-dir)))
        
        (unless (file-exists-p target-file)
          (message "Downloading extensions manifest version %s..." sha)
          (make-directory sha-dir t)
          (url-copy-file file-url target-file t)xa
          (setq manifest-updated t)
          (message "Manifest downloaded."))
        
        ;; Update symlink
        (let ((default-directory extensions-dir))
          (ignore-errors (delete-file "current"))
          (make-symbolic-link sha "current" t))
        
        (delete-file temp-sha)

        ;; Fetch repository content if needed
        (let ((repo-dir (expand-file-name "extensions" sha-dir)))
          (when (or manifest-updated (not (file-exists-p repo-dir)))
            (message "Downloading extensions repository...")
            (let* ((repo-url "https://github.com/poly-repo/neo-extensions/archive/refs/heads/main.tar.gz")
                   (tar-file (make-temp-file "neo-repo-" nil ".tar.gz")))
              (url-copy-file repo-url tar-file t)
              (message "Extracting extensions to %s..." repo-dir)
              (when (file-exists-p repo-dir)
                (delete-directory repo-dir t))
              (make-directory repo-dir t)
              (let ((exit-code (call-process "tar" nil nil nil "xf" tar-file "-C" repo-dir "--strip-components=2")))
                (if (zerop exit-code)
                    (message "Extensions repository installed.")
                  (message "Error extracting tarball: exit code %d" exit-code)))
              (delete-file tar-file))))
        
        target-file))))

(defun neo/fetch-extensions-config ()
  "Load user-specific configuration from the cache directory.
If the config file does not exist, it displays a welcome message."
  (let* ((filename (expand-file-name (format "~/.cache/%s/config.el"))))
    (if (not (file-exists-p filename))
	(message "Launch Welcome")	;welcome should be able to handle existing files as well
      (load filename))))

(defvar neo/extensions-lock-file
  (expand-file-name "neo_extensions.lock" "/tmp/")
  "Temporary lock file to indicate extensions are being fetched/loaded.")

(defun neo/maybe-fetch-extensions ()
  "Fetch extensions only if the lock file doesn't exist.
If fetching occurs, creates the lock file during the operation and
removes it afterward."
  (interactive)
  (if (file-exists-p neo/extensions-lock-file)
      (message "[neo] Extensions lock file exists, skipping fetch")
    (progn
      ;; Create lock file
      (write-region "" nil neo/extensions-lock-file)
      (unwind-protect
          ;; Call the real fetch function
          (neo/fetch-extensions)
        ;; Ensure the lock is removed afterward
        (when (file-exists-p neo/extensions-lock-file)
          (delete-file neo/extensions-lock-file))))))


;;; New implementation

(cl-defstruct neo--extension-registry
  "Represents the source location of an extension repository."
  name
  url
  override)

(defun neo/get-remote-commit-sha (registry)
  "Fetch the latest commit SHA for REGISTRY from its remote URL."
  (let* ((url (neo--extension-registry-url registry))
         ;; Run git ls-remote and capture the output
         (output (shell-command-to-string 
                  (format "git ls-remote %s main" url))))
    (if (string-match "^\\([a-f0-9]\\{40\\}\\)" output)
        (match-string 1 output)
      (error "Could not retrieve SHA for %s" url))))

(defun neo--archive-url (registry commit-sha)
  (let* ((repo-url (neo--extension-registry-url registry))
	 (base-url (replace-regexp-in-string "\\.git\\'" "" repo-url))
         (download-url (format "%s/archive/%s.tar.gz" base-url commit-sha)))
    download-url))

;; TODO maybe we should add commit-sha ourselves
(defun neo--download-github-tarfile (registry commit-sha target-dir)
  "Download REPO-URL at COMMIT-SHA, untar to TARGET-DIR stripping STRIP-LEVELS.

We assume that target-dir contains commit-sha"
  (if (file-directory-p target-dir)
      (message "Registry content already exists at %s; skipping download." target-dir)
(make-directory target-dir t)
(let ((temp-file (make-temp-file "neo-" nil ".tar.gz")))
  (unwind-protect
      (progn
        ;; --- BODY ---
        ;; 1. Download to temp-file
        ;; 2. Untar temp-file
	(url-copy-file (neo--archive-url registry commit-sha) temp-file t)
	(let ((default-directory (expand-file-name target-dir)))
	  (message "Expanding in %s" default-directory)
	  (call-process "tar" nil nil nil 
	                "-xzf" temp-file 
	                "--strip-components=3")))
    (when (file-exists-p temp-file)
      (delete-file temp-file)
      (message "Deleted %s" temp-file))))))

;; TODO we should probably update a symlink to current and return that instead
(defun neo/download-registry-content (registry commit-sha)
  (let ((content-directory (neo/cache-file-path (format "extensions/%s/%s" (neo--extension-registry-name registry) commit-sha))))
    (neo--download-github-tarfile registry commit-sha content-directory)
    content-directory))

(defun neo/download-latest-registry-content (registry)
  "Download the full content of the registry, unless an override is provided."
  (if (neo--extension-registry-p registry)
      (if (neo--extension-registry-override registry)
	  (neo--extension-registry-override registry)
	(neo/download-registry-content registry (neo/get-remote-commit-sha registry)))
    (error "Object is not a neo--extension-registry struct")))

(defvar neo/extension-registry-alist nil
  "Master list of all extension registries.")

(defun neo/register-registry (name url &optional override)
  "Create a new registry struct and add it to `neo/extension-registry-alist`."
  (let ((registry (make-neo--extension-registry
                   :name name
                   :url url
                   :override override)))
    ;; Use push or setf with alist-get to update the global list
    (setf (alist-get name neo/extension-registry-alist nil nil #'string=) 
          registry)
    registry))

;; TODO this probably goes elsewhere
(neo/register-registry 
 "neo" 
 "https://github.com/poly-repo/neo-extensions.git"
 (when (string= (neo/get-emacs-instance-name) "neo-devel")
   (expand-file-name "extensions/extensions/neo" user-emacs-directory)))

(defun neo--populate-extensions-from-file (file extensions-table)
  "Load FILE and populate EXTENSIONS-TABLE. Return t if successful."
  (let ((file-path (expand-file-name file user-emacs-directory)))
    (when (file-exists-p file-path)
      ;; We bind neo--extensions locally so the loaded file 
      ;; (which uses sethash indirectly) targets our table.
      (dlet ((neo--extensions extensions-table)
	     (neo--extensions-emblem-path (expand-file-name "emblem64.png" (file-name-directory file-path))))
        (load file-path t t)))))

(defun neo--load-all-sub-manifests (root-dir combined-table)
  "Scan ROOT-DIR/*/manifest.el and add extensions to COMBINED-TABLE."
  (let ((full-root (expand-file-name root-dir)))
    (when (file-directory-p full-root)
      ;; 1. Get all subdirectories (excluding . and ..)
      (let ((subdirs (directory-files full-root t "^[^.]")))
        (dolist (dir subdirs)
          (when (file-directory-p dir)
            (let ((manifest-path (expand-file-name "manifest.el" dir)))
              ;; 2. If manifest exists, populate the table
              (when (file-exists-p manifest-path)
                (neo--populate-extensions-from-file manifest-path combined-table)))))))
    combined-table))

(defun neo--load-local-manifest (registry combined-table)
  (let ((root-dir (neo--extension-registry-override registry)))
    (neo--load-all-sub-manifests root-dir combined-table)))

(defun neo--load-remote-manifest (registry combined-table)
  (let* ((repo-url (neo--extension-registry-url registry))
	 (base-url (replace-regexp-in-string "\\.git\\'" "" repo-url))
         (download-url (format "%s/releases/download/latest/neo-extensions.el" base-url))
	 (local-file-path (neo/cache-file-path "neo-extensions.el")))
    (url-copy-file download-url (neo/cache-file-path "neo-extensions.el") t)
    (neo--populate-extensions-from-file local-file-path combined-table)))

(defun neo--load-combined-extensions-manifest (registry-alist)
  "Load the digest for all available estensions in REGISTRY-ALIST."
  (let ((combined-table (make-hash-table :test 'equal)))
    (cl-loop for (name . registry) in registry-alist
             do
	     (if (neo--extension-registry-override registry)
		 (neo--load-local-manifest registry combined-table)
	       (neo--load-remote-manifest registry combined-table)))
    combined-table))


;;; For now we support a single registry. Later we'll traverse the
;;; digest looking for dependencies in other registries. As long as we
;;; don't have cycles in dependencies between extensions we should be
;;; ok with registries circularly depending on each other.

(defun neo/load-registry-content (registry-name)
  (let ((registry (alist-get registry-name neo/extension-registry-alist nil nil #'string=)))
    (neo/download-latest-registry-content registry)))

(provide 'neo-extensions)

