(require 'cl-lib)
(require 'cl-generic)


(defgroup neo-extensions nil
  "Settings for Neo Extensions system."
  :group 'neo
  :prefix "neo/extensions-")

(cl-defstruct neo/repository
  type
  url
  path)

;; (cl-defstruct neo/extension
;;   name
;;   publisher
;;   emblem
;;   description
;;   categories
;;   keywords
;;   requires
;;   repository)

(cl-defstruct (neo/extension
               (:copier nil))
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
  ;; background matching hilight. Changes with theme setting, need to
  ;; register a hook for that.
)


(cl-defgeneric neo/render (object)
  "Render OBJECT at point and return a point marker.")

;; (cl-defmethod neo/render ((ext neo/extension))
;;   "Render a `neo/extension` EXT at point and return a point marker."
;;   (let ((start (point)))
;;     ;; Emblem (non-cursor-highlighted)
;;     (when-let ((emblem (neo/extension-emblem ext)))
;;       (when (stringp emblem)
;;         (insert (propertize " "
;;                             'display (create-image emblem 'png t)
;;                             'cursor nil))
;;         (insert " ")))

;;     ;; Name
;;     (insert (propertize (or (neo/extension-name ext) "Unnamed")
;;                         'face '(:weight bold :height 1.2)))
;;     (insert "\n\n")

;;     ;; Description
;;     (when-let ((desc (neo/extension-description ext)))
;;       (insert (propertize desc 'face '(:slant italic :height 0.95)))
;;       (insert "\n\n"))

;;     ;; Publisher
;;     (when-let ((pub (neo/extension-publisher ext)))
;;       (insert (format "%-12s: %s\n" "Publisher" pub)))

;;     ;; Repository details
;;     (when-let ((repo (neo/extension-repository ext)))
;;       (neo/render repo))  ;; delegate to repo method

;;     ;; Divider
;;     (insert "\n" (make-string 60 ?─) "\n\n")

;;     (add-text-properties start (point)
;;                          `(neo-extension ,ext
;;                                          mouse-face highlight
;;                                          help-echo "RET or click to view details"))
;;     (point-marker)))


(cl-defmethod neo/render-details ((ext neo/extension))
  "Render EXT in the current buffer. Return (start . end) position."
  (when-let ((emblem (neo/extension-emblem ext)))
    (when (stringp emblem)
      (insert-image (create-image emblem 'png t))
      (insert " ")))

    ;; Title
    (insert (propertize (or (neo/extension-name ext) "Unnamed")
                        'face '(:weight bold :height 1.5)))
    (insert "\n\n")
)

;; (cl-defmethod neo/render ((ext neo/extension))
;;   "Render EXT in the current buffer. Return (start . end) position."
;;   (let ((start (point)))
;;     ;; Insert emblem
;;     (when-let ((emblem (neo/extension-emblem ext)))
;;       (when (stringp emblem)
;;         (insert-image (create-image emblem 'png t))
;;         (insert " ")))

;;     ;; Title
;;     (insert (propertize (or (neo/extension-name ext) "Unnamed")
;;                         'face '(:weight bold :height 1.2)))
;;     (insert "\n\n")
;;     ;; Description
;;     (when-let ((desc (neo/extension-description ext)))
;;       (insert (propertize desc 'face '(:slant italic :height 0.95)))
;;       (insert "\n"))
;;     (insert "\n")
;;     ;; Info lines
;;     (let ((repo (neo/extension-repository ext)))
;;       (dolist (pair `(("Publisher" ,(neo/extension-publisher ext))
;;                       ("URL" ,(neo/repository-url repo))
;;                       ("Path" ,(neo/repository-path repo))))
;; 	        (when (cadr pair)
;;           (insert (format "%-10s: %s\n" (car pair) (cadr pair))))))

;;     ;; Divider
;;     (insert (make-string (- (window-text-width) 0) ?─))
;;     (insert "\n")



;;     ;; Store text property for activation
;;     (let ((end (save-excursion
;;              (goto-char start)
;;              (when (re-search-forward "^─+$" nil t)
;;                (beginning-of-line))
;;              (point))))
;;       (put-text-property start end 'neo-extension ext)
;;       (cons start end))))
(cl-defmethod neo/render ((ext neo/extension))
  "Render EXT in the current buffer. Return (start . end) position."
  (let ((start (point)))

    ;; Insert image using overlay
    (when-let ((emblem (neo/extension-emblem ext)))
      (when (stringp emblem)
        (let ((img (create-image emblem 'png t)))
          ;; Insert a space and record its bounds *after* insertion
          (insert " ")
          (let ((ov (make-overlay (1- (point)) (point))))
            (overlay-put ov 'display img)
            (overlay-put ov 'neo-image t)
            ;; Store it in the extension struct
            (setf (neo/extension-summary-overlay ext) ov)))))
    
    (insert " ") ;; optional visual spacing

    ;; Title
    (insert (propertize (or (neo/extension-name ext) "Unnamed")
                        'face '(:weight bold :height 1.2)))
    (insert "\n\n")

    ;; Description
    (when-let ((desc (neo/extension-description ext)))
      (insert (propertize desc 'face '(:slant italic :height 0.95)))
      (insert "\n\n"))

    ;; Info block
    (let ((repo (neo/extension-repository ext)))
      (dolist (pair `(("Publisher" ,(neo/extension-publisher ext))
                      ("Type" ,(neo/repository-type repo))
                      ("URL" ,(neo/repository-url repo))
                      ("Path" ,(neo/repository-path repo))))
        (when (cadr pair)
          (insert (format "%-10s: %s\n" (car pair) (cadr pair))))))

    ;; Divider
    (let ((width (1- (or (window-body-width nil t) 80))))
      (insert (make-string width ?─) "\n"))

    ;; Final range
    (let ((end (point)))
      (put-text-property start end 'neo-extension ext)
      (cons start end))))





;; (cl-defmethod neo/render ((ext neo/extension))
;;   "Render EXT in the current buffer. Return (start . end) position."
;;   (let* ((emblem (neo/extension-emblem ext))
;; 	 (img (create-image emblem 'png t)))
;;     (let ((start (point)))
;;       (insert " ") ; insert a space
;;       (add-text-properties start (point) `(display ,img rear-nonsticky t cursor-intangible t invisible nil))
;;       ;; Title
;;       (insert (propertize (or (neo/extension-name ext) "Unnamed")
;;                           'face '(:weight bold :height 1.2)))
;;       (insert "\n\n")
;;       ;; Description
;;       (when-let ((desc (neo/extension-description ext)))
;; 	(insert (propertize desc 'face '(:slant italic :height 0.95)))
;; 	(insert "\n"))
;;       (insert "\n")
;;       ;; Info lines
;;       (let ((repo (neo/extension-repository ext)))
;; 	(dolist (pair `(("Publisher" ,(neo/extension-publisher ext))
;; 			("Type" ,(neo/repository-type repo))
;; 			("URL" ,(neo/repository-url repo))
;; 			("Width" ,(window-text-width))
;; 			("Path" ,(neo/repository-path repo))))
;; 	  (when (cadr pair)
;;             (insert (format "%-10s: %s\n" (car pair) (cadr pair))))))
;;       ;; Divider
;;       (insert (make-string (- (window-text-width) 0) ?─))
;;       (insert "\n")
;;       ;; Store text property for activation
;;       (let ((end (save-excursion
;; 		   (goto-char start)
;; 		   (when (re-search-forward "^─+$" nil t)
;; 		     (beginning-of-line))
;; 		   (point))))
;; 	(put-text-property start end 'neo-extension ext)
;; 	(cons start end)))))


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

(defvar neo/extensions (make-hash-table :test #'equal)
  "Hash table mapping publisher/name to `neo/extension` instances.")

(defun neo/dump-extension-names-and-descriptions (extensions)
  "Display names and descriptions from `neo/extensions` in a temporary buffer."
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


(defun neo/normalize-name (name)
  "Normalize NAME by downcasing and replacing spaces with dashes."
  (replace-regexp-in-string
   " " "-"
   (downcase name)))

(defun neo/extension-blurb (ext)
  (format "%s/%s" (neo/extension-publisher ext) (neo/normalize-name (neo/extension-name ext))))

;;; TODO replace blurb with slug

(defmacro neo/extension (&rest args)
  "Register a new Neo extension and store it in `neo/extensions`.

Args:
- :name NAME (string)
- :publisher PUBLISHER (string)
- :description DESCRIPTION (string)
- :categories (LIST of symbols)
- :keywords (LIST of symbols)
- :requires (LIST of symbols or a single symbol)
- :repository (plist with :type, :url, :path)"
  (let* ((name (plist-get args :name))
	 (title (plist-get args :title))
	 (normalized-name (neo/normalize-name name))
         (publisher (plist-get args :publisher))
         (desc (plist-get args :description))
         (cats (plist-get args :categories))
         (tags (plist-get args :keywords))
	 (emblem (plist-get args :emblem))
         (requires (plist-get args :requires))
         (provides (plist-get args :provides))
         (depends-on (plist-get args :depends-on))
	 (require-list (mapcar (lambda (x) (symbol-name x))
                               (if (listp requires) requires (list requires))))
         (provide-list (mapcar (lambda (x) (symbol-name x))
                               (if (listp provides) provides (list provides))))
         (depend-list (mapcar (lambda (x) (symbol-name x))
                              (if (listp depends-on) depends-on (list depends-on))))
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
         (blurb (neo/extension-blurb extension)))
    `(puthash ,blurb ,extension neo/extensions)))

(defun neo/load-extension-manifests (extensions-summary-file)
  (load extensions-summary-file)
  neo/extensions)

  
(defun neo/sorted-extensions-by-name (extensions)
  "Return a list of `neo/extension` values sorted by name."
  (let (extension-list)
    (maphash (lambda (_k v) (push v extension-list)) extensions)
    (sort extension-list
          (lambda (a b)
            (string< (neo/extension-name a)
                     (neo/extension-name b))))))

(defun neo/load-extension (ext)
  (let* ((publisher (neo/extension-publisher ext))
	 (name (neo/normalize-name (neo/extension-name ext)))
	 (file (format "%s/extensions/%s/%s/%s.el" user-emacs-directory publisher name name)))
    (message (format "Loading %s/%s from %s" publisher name file))))

(defun neo/load-extensions (extensions)
  (let ((extensions (neo/sorted-extensions-by-name  extensions)))
    (mapcar #'neo/load-extension extensions)))

(setq extensions (neo/load-extension-manifests "~/Projects/uno/neo-extensions.el"))
(neo/dump-extension-names-and-descriptions extensions)
(neo/load-extensions extensions)

;; (require 'neo-extensions-summary)

;; (defun neo/sorted-extensions-by-name ()
;;   "Return a list of `neo/extension` values sorted by name."
;;   (let (extensions)
;;     (maphash (lambda (_k v) (push v extensions)) neo/extensions)
;;     (sort extensions
;;           (lambda (a b)
;;             (string< (neo/extension-name a)
;;                      (neo/extension-name b))))))

;; (neo/extensions-summary-open-buffer (neo/sorted-extensions-by-name))


(provide 'neo-extensions)


;; ;-----------------------------------------------------------------------------

;; (defgroup neo-extensions nil
;;   "Settings for Neo Extensions system."
;;   :group 'neo
;;   :prefix "neo/extensions-")


;; (cl-defstruct neo-extension
;;   name author forge user repo path description emblem)

;; (require 'neo-extensions-summary)

;; (defcustom neo-extensions-cache-directory
;;   (let ((xdg-cache (getenv "XDG_CACHE_HOME")))
;;     (expand-file-name "neo/"
;;                       (or xdg-cache
;;                           (expand-file-name "~/.cache/"))))
;;   "Directory where the neo-extensions cache file will be stored."
;;   :type 'directory
;;   :group 'neo-extensions)

;; (defconst neo-extensions-cache-filename "neo-extensions.el"
;;   "The fixed filename used to cache the extensions data.")

;; (defun neo-extensions-cache-path ()
;;   "Return the full path to the neo-extensions cache file."
;;   (expand-file-name neo-extensions-cache-filename neo-extensions-cache-directory))

;; (defun neo/read-extensions-from-file (file)
;;   "Return a list of (neo/extension ...) forms from FILE."
;;   (with-temp-buffer
;;     (insert-file-contents-literally file)
;;     (let (results)
;;       (condition-case nil
;;           (while t
;;             (push (read (current-buffer)) results))
;;         (end-of-file (nreverse results))))))

;; (defun neo/extension-plist (ext)
;;   "Return the plist of a neo/extension form EXT."
;;   (cdr ext))

;; (defun neo/render-extension (plist)
;;   "Insert a formatted block for one extension described by PLIST."
;;   (let ((start (point)))
;;     (let ((emblem (plist-get plist :emblem)))
;;       (when (and emblem (stringp emblem))
;; ;        (insert-image (create-image emblem 'png t))
;; 	(insert (propertize " " 'display (create-image emblem 'png t)))
;;         (insert " ")))
;;     (insert (propertize (or (plist-get plist :name) "Unnamed")
;;                         'face '(:weight bold :height 1.2)))
;;     (insert "\n\n")
;;     (when-let* ((desc (plist-get plist :description)))
;;       (insert (propertize desc 'face '(:slant italic :height 0.95)))
;;       (insert "\n"))
;;     (insert "\n")
;;     (dolist (key '(:author :forge :user :repo :path))
;;       (when-let* ((val (plist-get plist key)))
;;         (insert (format "%-10s: %s\n" (substring (symbol-name key) 1) val))))
;;     (insert "\n")
;;     (insert (make-string 60 ?─))
;;     (insert "\n\n")))

;; (defun neo/show-extensions-buffer (file)
;;   "Parse and display neo/extension records from FILE in a nicely formatted buffer."
;;   (interactive "fPath to neo-extensions.el: ")
;;   (let* ((extensions (neo/read-extensions-from-file file))
;;          (buf (get-buffer-create "*Neo Extensions*")))
;;     (with-current-buffer buf
;;       (read-only-mode -1)
;;       (erase-buffer)
;;       (dolist (ext extensions)
;;         (neo/render-extension (neo/extension-plist ext)))
;;       (goto-char (point-min))
;;       (read-only-mode 1)
;;       (special-mode))
;;     (display-buffer buf)))

;; (neo/show-extensions-buffer "~/Projects/uno/neo-extensions.el")

;; (require 'wid-edit)

;; (defun neo-extensions-settings-buffer ()
;;   "Display a buffer with custom widgets like `customize-group`."
;;   (interactive)
;;   (let ((buf (get-buffer-create "*Neo Extensions Settings*")))
;;     (with-current-buffer buf
;;       (kill-all-local-variables)
;;       (let ((inhibit-read-only t))
;;         (erase-buffer))
;;       (remove-overlays)
;;       (widget-insert "Configure Neo Extensions:\n\n")

;;       ;; Example: directory selector
;;       (widget-create 'file
;;                      :tag "Cache Directory"
;;                      :format "%t: %v\n"
;;                      :must-match t
;;                      :size 40
;;                      :value neo-extensions-cache-directory
;;                      :notify (lambda (widget &rest _ignore)
;;                                (setq neo-extensions-cache-directory (widget-value widget))))

;;       ;; Example: toggle
;;       (widget-create 'checkbox t)
;;       (widget-insert " Enable something\n")

;;       ;; Save button
;;       (widget-create 'push-button
;;                      :notify (lambda (&rest _)
;;                                (message "Saved: %s" neo-extensions-cache-directory))
;;                      "Save")

;;       (use-local-map widget-keymap)
;;       (widget-setup))
;;     (switch-to-buffer buf)))

;; (neo-extensions-settings-buffer)

;; 					;--------------------------------------------------------------------------------------
;; (defvar neo/saved-window-configuration nil
;;   "Holds the saved window configuration before custom layout.")

;; (defcustom neo/sidebar-ratio 0.33
;;   "Ratio (0 < RATIO < 1) of the left sidebar when splitting the frame."
;;   :type 'float
;;   :group 'neo-extensions)

;; (defun neo/setup-side-split ()
;;   "Save the current window configuration and split the frame side-by-side.
;; Left window will be `neo/sidebar-ratio` of the frame width."
;;   (interactive)
;;   (when (bound-and-true-p golden-ratio-mode)
;;     (golden-ratio-mode -1))
;;   ;; Save current window configuration
;;   (setq neo/saved-window-configuration (current-window-configuration))

;;   ;; Clear current layout
;;   (delete-other-windows)

;;   ;; Split window vertically (side-by-side)
;;   (let* ((total-width (window-total-width))
;;          (left-width (floor (* total-width (- 1.0 neo/sidebar-ratio))))
;;          (left-window (selected-window))
;;          (right-window (split-window left-window left-width t))) ;; 't' means horizontally
;;     ;; Optionally, you can switch to buffers in each window here
;;     (select-window left-window)))

;; (defun neo/restore-window-configuration ()
;;   "Restore the previously saved window configuration."
;;   (interactive)
;;   (when neo/saved-window-configuration
;;     (set-window-configuration neo/saved-window-configuration)))

;; (provide 'neo-extensions)
