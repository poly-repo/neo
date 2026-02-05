(require 'cl-lib)

(defconst neo--core-dir (file-name-directory (or load-file-name buffer-file-name)))


(defvar neo/--emacs-instance-name-cache nil
  "Cached result of `neo/get-emacs-instance-name`.")

(defun neo/get-emacs-instance-name ()
  "Return the Emacs instance name from `--name`, or fallback to `emacs`.

Reads from /proc/self/cmdline if needed (Linux-only)."
  (or neo/--emacs-instance-name-cache
      (setq neo/--emacs-instance-name-cache
            (or
             ;; Try $EMACS_NAME for compatibility or scripting
             (getenv "EMACS_NAME")
             ;; Try /proc/self/cmdline
             (when (and (eq system-type 'gnu/linux)
                        (file-readable-p "/proc/self/cmdline"))
               (with-temp-buffer
                 (insert-file-contents-literally "/proc/self/cmdline")
                 (let* ((args (split-string (buffer-string) "\0" t))
                        (index (cl-position "--name" args :test #'string=)))
                   (when index
                     (nth (1+ index) args)))))
             ;; Fallback
             "neo"))))

(defun neo/load-file (absolute-path &optional failure-ok)
  "Load the file at ABSOLUTE-PATH.

On success, return t.

On failure (file missing, unreadable, or LOAD errors):
- If FAILURE-OK is nil (default), signal an error.
- If FAILURE-OK is non-nil, print a message and return nil.

This wrapper gives consistent behavior for all load failures without
distinguishing between missing and unreadable files."
  (unless (and (stringp absolute-path)
               (file-name-absolute-p absolute-path))
    (error "neo/load-file: ABSOLUTE-PATH must be an absolute filename (got %S)"
           absolute-path))
  (let ((success
         (condition-case err
             (and (file-readable-p absolute-path)
                  ;; Normalize successful loads to t
                  (load absolute-path nil 'nomessage)
                  t)
           (error
            (message "neo: error loading %s: %s"
                     absolute-path (error-message-string err))
            nil))))
    (unless success
      (if (not failure-ok)
          (error "neo: failed to load %s" absolute-path)))
    (message "Loading %s...%s"
             absolute-path
             (if success "success" "failure (ignored)"))
    success))

(defun neo/config-file-path (filename)
  (let* ((current-profile (expand-file-name "current-profile"
                                            neo/config-directory))
         (absolute-path (expand-file-name filename current-profile)))
    absolute-path))
  
(defun neo/load-config-file (filename &optional failure-ok)
  "Load FILENAME from the current profile directory.

Returns t on success, nil on failure (if FAILURE-OK is non-nil), and
signals otherwise. Delegates to `neo/load-file`."
  (neo/load-file (neo/config-file-path filename) failure-ok))

(defun neo/cache-file-path (filename)
  (expand-file-name filename neo/cache-directory))

(defun neo/load-cached-file (filename &optional failure-ok)
  "Load FILENAME from `neo/cache-directory`.

Returns t on success, nil on failure (if FAILURE-OK is non-nil), and
signals otherwise. Delegates to `neo/load-file`."
  (let ((absolute-path (neo/cache-file-path filename)))
    (message "LOADING: %s" absolute-path)
    (neo/load-file absolute-path failure-ok)))

(defconst neo--data-dir
  (expand-file-name "neo/"
		    (or (getenv "XDG_DATA_HOME")
			"~/.local/share")))

(defun neo/data-file-path (filename)
  (expand-file-name filename neo--data-dir))

(defun neo/paraphenalia (thing)
  "Return non-nil if THING should be displayed according to `neo/paraphenalia-list`.

THING is a symbol identifying a UI element that may be toggled on or
off.  Expected values include:

  - `neo/paraphenalia-scrollbar`
  - `neo/paraphenalia-toolbar`
  - `neo/paraphenalia-menu`
  - `neo/paraphenalia-advertisement`
  - `neo/paraphenalia-scratch-message`

The variable `neo/paraphenalia-list` controls which items should be
shown.  It may take one of the following forms:

  • The symbol `neo/paraphenalia-all`
    Meaning that *all* UI elements are displayed, regardless of THING.

  • A list of symbols
    Only those symbols explicitly listed are displayed.  THING is
    considered enabled when it appears in this list (tested with `memq`).

  • An empty list
    No UI elements are displayed.

Return value is non-nil if THING should be shown, nil otherwise."
  (or (eq neo/paraphenalia-list 'neo/paraphenalia-all)
      (memq thing neo/paraphenalia-list)))

(defun neo/pretend-new-user ()
  "Reset configuration and pretend to be a new user.

╭──────────────────────────────────────────────────────────╮
│ SQL: DELETE FROM config WHERE key = 'enabled-extensions' │
╰──────────────────────────────────────────────────────────╯"
  (interactive)
  (require 'neo-config)
  (sqlite-execute neo/config-db-handle "DELETE FROM config WHERE key = ?" '("enabled-extensions"))
  (neo/set-config "pretend-new-user" "t")
  (message "Enabled extensions reset and pretend-new-user set to t. Restart Emacs to see the new user experience."))

(defun neo/full-monty-confirm (_button)
  "Actually enable full monty mode and restart Emacs."
  (require 'neo-config)
  (neo/set-config "enabled-extensions" "(\"neo:full-monty\")")
  (neo/set-config "pretend-new-user" "nil")
  (neo/set-config "paraphenalia-config" "()")
  (if (fboundp 'restart-emacs)
      (restart-emacs)
    (save-buffers-kill-emacs)))

(defun neo--make-image-label (filename &optional scale)
  "Create a display label from FILENAME in the NEO assets directory.
Optional SCALE defaults to 1.0."
  (let ((img (create-image (expand-file-name filename neo--core-dir) nil nil 
                           :scale (or scale 1.0) 
                           :ascent 'center)))
    (propertize " " 'display img 'rear-nonsticky t)))

(defvar neo--intro "
The Red Pill: Full NEO initiation. Unleash a YOLO-tuned, monorepo-shredding environment where the Elisp is hot and the constraints are non-existent.\n\nThe Blue Pill: Return to Vanilla Emacs. A safe, beige, and blissfully \"productive\" purgatory where nothing exciting ever happens. You can still install add-ons manually, but you’ll be doing it in a world of lukewarm water and default keybindings.
")

(defun neo/full-monty (_button)
  "Ask for confirmation before going full monty."
  (let ((buf (get-buffer-create "*Neo YOLO*"))
        (image-file (expand-file-name "yolo.png" neo--core-dir))
        (wconf (current-window-configuration)))
    (with-current-buffer buf
      (erase-buffer)
      (setq cursor-type nil)
      (insert-button (neo--make-image-label "red-pill128.png" .8) ;"[ RED PILL ]"
		     'action #'neo/full-monty-confirm
		     'follow-link t
		     'face 'default      ; Removes the underline
                     'mouse-face nil     ; Removes the hover highlighting
		     'help-echo "Wake up, Neo.")
      (when (file-exists-p image-file)
        (insert-image (create-image image-file)))
      (insert-button (neo--make-image-label "blue-pill128.png" .5) ;"[ BLUE PILL ]"
		     'action (lambda (b)
                               (let ((saved-wconf (button-get b 'wconf)))
				 (kill-buffer (current-buffer))
				 (when saved-wconf (set-window-configuration saved-wconf))))
		     'face 'default      ; Removes the underline
                     'mouse-face nil     ; Removes the hover highlighting
		     'follow-link t
		     'wconf wconf
		     'help-echo "Back to sleep.")
      (insert "\n\n")
      (insert neo--intro)
      (switch-to-buffer buf)
      (delete-other-windows))))

(defun neo/fancy-splash--replace-args (orig-fun &rest args)
  "Replace arguments to `fancy-splash-insert` if the second argument is the \"To start\" string."
  (if (and (> (length args) 1)
           (string-match-p "To start" (nth 2 args)))
      (let ((separator (concat (make-string 60 ?─) "\n")))
        ;; Replace with your custom args
        (apply orig-fun
               `("\n" :face default ,separator
                 :face (:inherit variable-pitch :weight bold) "Welcome to Neo!\t"
                 :link ("Start configuration" ,(lambda (_b) (message "Clicked")) "Help" (:weight bold))
	         "\t"
                 :link ("Don't do this 😜" ,#'neo/full-monty "Go full Monty")
		 "\n"
					;                       :face variable-pitch "\n\nTo quit a partially entered command, type Control-g.\n"
                 :face default ,separator)))
    ;; Otherwise call original function
    (apply orig-fun args)))

(defun neo/normal-splash-screen-extra (&rest _args)
  "Add Neo welcome message to the text mode splash screen."
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    (let ((separator (concat (make-string 60 ?─) "\n")))
      (insert "\n" separator)
      (insert "Welcome to Neo!\n\n")
      (insert-button "Start configuration"
                     'action (lambda (_b) (message "Clicked"))
                     'follow-link t)
      (insert "    ")
      (insert-button "Don't do this 😜"
                     'action #'neo/full-monty
                     'follow-link t)
      (insert "\n" separator "\n"))))

(advice-add 'fancy-splash-insert :around #'neo/fancy-splash--replace-args)
(advice-add 'normal-splash-screen :after #'neo/normal-splash-screen-extra)

(provide 'neo-early-init-utils)
