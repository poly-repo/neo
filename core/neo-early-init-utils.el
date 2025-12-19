(require 'cl-lib)

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

(defun neo/fancy-splash--replace-args (orig-fun &rest args)
  "Replace arguments to `fancy-splash-insert` if the second argument is the \"To start\" string."
  (if (and (> (length args) 1)
           (string-match-p "To start" (nth 2 args)))
      ;; Replace with your custom args
      (apply orig-fun
             `(:face variable-pitch
               "\nWelcome to Neo!\t"
               :link ("Start configuration" ,(lambda (_b) (message "Clicked") :face (:weight bold)) "Help")
	       "\t"
               :link ("Don't do this 😜" ,(lambda (_b) (message "Clicked") :face (:weight bold)) "Go full Monty")
               "\n"))
    ;; Otherwise call original function
    (apply orig-fun args)))

;; Add the advice
(advice-add 'fancy-splash-insert :around #'neo/fancy-splash--replace-args)

(provide 'neo-early-init-utils)
