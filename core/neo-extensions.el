;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'cl-generic)

(defgroup neo-extensions nil
  "Settings for Neo Extensions system."
  :group 'neo
  :prefix "Neo/extensions-")

(cl-defstruct neo/repository
  type
  url
  path)

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
  ;; background matching highlight. Changes with theme setting, need to
  ;; register a hook for that.
)


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

;(neo/use-package posframe :ensure t)
;(require 'posframe)

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
    
    ;; ;; Insert image using overlay
    ;; (when-let ((emblem (neo/extension-emblem ext)))
    ;;   (when (stringp emblem)
    ;;     (let ((img (create-image emblem 'png t)))
    ;;       ;; Insert a space and record its bounds *after* insertion
    ;;       (insert " ")
    ;;       (let ((ov (make-overlay (1- (point)) (point))))
    ;;         (overlay-put ov 'display img)
    ;;         (overlay-put ov 'neo-image t)
    ;;         ;; Store it in the extension struct
    ;;         (setf (neo/extension-summary-overlay ext) ov)))))
    
    (insert " ") ;; optional visual spacing

    ;; Title
    ;; (let ((info '(("Publisher" . "neo")
    ;; 		  ("Type" . "git")
    ;; 		  ("URL" . "https://github.com/poly-repo/neo-extensions.git")
    ;; 		  ("Path" . "extensions/uno/news"))))
    ;;   (insert
    ;;    (propertize "❓"
    ;; 		   'help-echo (neo/make-hover-callback info)
    ;; 		   'mouse-face 'default ; 'highlight
    ;; 		   'keymap neo/info-icon-map)))
    
    (insert (propertize (or (neo/extension-title ext) "Unnamed")
                        'face '(:weight bold :height 1.2)))
    (insert "\n\n")

    ;; Description
    (when-let ((desc (neo/extension-description ext)))
      (insert (propertize desc 'face '(:slant italic :height 0.95)))
      (insert "\n\n"))


    ;; ;; Info block
    ;; (let ((repo (neo/extension-repository ext)))
    ;;   (dolist (pair `(("Publisher" ,(neo/extension-publisher ext))
    ;;                   ("Type" ,(neo/repository-type repo))
    ;;                   ("URL" ,(neo/repository-url repo))
    ;;                   ("Path" ,(neo/repository-path repo))))
    ;;     (when (cadr pair)
    ;;       (insert (format "%-10s: %s\n" (car pair) (cadr pair))))))

    ;; Divider
    ;; (let ((width (1- (or (window-body-width nil t) 80))))
    ;;   (insert (make-string width ?─) "\n"))
    ;; (insert
    ;;  (propertize " "
    ;; 		 'display '(space :align-to right)
    ;; 		 'face '(:background "gray30")))
    ;; (insert "\n")
    (neo/insert-thin-divider)
    
    ;; Final range
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

;; (defun neo/insert-thin-divider (&optional color)
;;   "Insert a thin horizontal divider using underline, aligned to window width.
;; If COLOR is nil, use the theme's default foreground color."
;;   (let* ((line-color (or color (face-foreground 'default nil t)))
;;          (bg-color   (face-background 'default nil t)))
;; ;    (insert "\n")
;;     (insert
;;      (propertize " "
;;                  'display '(space :align-to right)
;;                  'face `(:overline ,line-color
;;                          :foreground ,bg-color
;;                          :background ,bg-color)))
;;     (insert "\n")))

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
  "Hash table mapping publisher/name to `neo/extension` instances.")

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


(defun neo--normalize-name (name)
  "Normalize NAME by downcasing and replacing spaces with dashes."
  (replace-regexp-in-string
   " " "-"
   (downcase name)))

(defun neo--extension-slug (ext)
  (format "%s/%s" (neo/extension-publisher ext) (neo--normalize-name (neo/extension-name ext))))

;;; TODO replace blurb with slug

(defmacro neo/extension (&rest args)
  "Register a new Neo extension and store it in `neo--extensions`.

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
	 (normalized-name (neo--normalize-name name))
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
         (blurb (neo--extension-slug extension)))
    `(puthash ,blurb ,extension neo--extensions)))

(defun neo--load-extension-manifests (extensions-summary-file)
  (load extensions-summary-file)
  neo--extensions)

  
(defun neo--sorted-extensions-by-name (extensions)
  "Return a list of `neo/extension` values sorted by name."
  (let (extension-list)
    (maphash (lambda (_k v) (push v extension-list)) extensions)
    (sort extension-list
          (lambda (a b)
            (string< (neo/extension-name a)
                     (neo/extension-name b))))))

;; (defun neo--load-extension (ext)
;;   (let* ((publisher (neo/extension-publisher ext))
;; 	 (name (neo--normalize-name (neo/extension-name ext)))
;; 	 (base (expand-file-name "extensions/" user-emacs-directory))
;;          (file (expand-file-name (format "%s/%s/%s.el" publisher name name) base)))
;;     (message (format "Loading %s/%s from %s" publisher name file))
;;     (load file)))

(defun neo--load-extension (ext)
  "Load the extension file for EXT.
Logs errors to *Messages* but never signals.  Returns non-nil on success."
  (let* ((publisher (neo/extension-publisher ext))
         (name      (neo--normalize-name (neo/extension-name ext)))
         (base      (expand-file-name "extensions/" user-emacs-directory))
         (file      (expand-file-name (format "%s/%s/%s.el" publisher name name) base)))
    (message "[neo] Loading %s/%s from %s" publisher name file)
    (let ((res (condition-case err
                   ;; NOERROR=t prevents file-not-found from signaling.
                   ;; NOMESSAGE='nomessage keeps `load` quiet; we log ourselves.
                   (load file t 'nomessage 'nosuffix)
                 (error
                  (message "[neo] Error while loading %s: %s"
                           file (error-message-string err))
                  :error))))
      (cond
       ;; evaluation error caught above
       ((eq res :error) nil)
       ;; file not found or unreadable (load returned nil with NOERROR=t)
       ((null res)
        (message "[neo] Extension file not found: %s" file)
        nil)
       ;; success (load returns non-nil)
       (t
        (message "[neo] Loaded %s" file)
        t)))))

(defun neo/load-extensions (extensions)
  (let ((extensions (neo--sorted-extensions-by-name  extensions)))
    (mapcar #'neo--load-extension extensions)))

;; TODO only fetch if older than X hours unless FORCE is used
(defun neo/fetch-extensions ()
  "Download and cache the latest neo-extensions.el if the SHA has changed.
Keeps a copy in ~/.cache/neo/"
  (let* ((base-url "https://github.com/poly-repo/neo-extensions/releases/download/latest/")
         (filename "neo-extensions.el")
         (sha-filename "neo-extensions.sha256")
	 (instance (neo/get-emacs-instance-name))
         (cache-dir (expand-file-name (format "~/.cache/%s/" instance)))
         (file-path (expand-file-name filename cache-dir))
         (sha-path (expand-file-name sha-filename cache-dir))
         (sha-latest-path (expand-file-name (concat sha-filename ".latest") cache-dir))
         (file-url (concat base-url filename))
         (sha-url (concat base-url sha-filename)))
    
    (unless (file-directory-p cache-dir)
      (make-directory cache-dir t))

    (url-copy-file sha-url sha-latest-path t)

    (let ((update-needed (or (not (file-exists-p file-path))
                             (not (file-exists-p sha-path))
                             (not (string= (with-temp-buffer
                                             (insert-file-contents sha-latest-path)
                                             (buffer-string))
                                           (with-temp-buffer
                                             (insert-file-contents sha-path)
                                             (buffer-string)))))))
      (when update-needed
        (message "Updating %s..." file-path)
        (url-copy-file file-url file-path t)
        (rename-file sha-latest-path sha-path t)
        (message "neo-extensions.el updated."))
      (unless update-needed
        (delete-file sha-latest-path)
        (message "neo-extensions.el is up to date.")))

    file-path))

(defun neo/fetch-extensions-config ()
  (let* ((filename (expand-file-name (format "~/.cache/%s/config.el"))))
    (if (not (file-exists-p filename))
	(message "Launch Welcome")	;welcome should be able to handle existing files as well
      (load filename))))

(neo/fetch-extensions)
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

(provide 'neo-extensions)

