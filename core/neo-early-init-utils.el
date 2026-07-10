;;; -*- lexical-binding: t -*-
(require 'cl-lib)

(defconst neo--core-dir (file-name-directory (or load-file-name buffer-file-name)))

(defvar neo--mode (getenv "NEO_MODE")
  "Value of the NEO_MODE environment variable at startup, or nil.")

(defvar neo/debug-p (or init-file-debug (equal neo--mode "debug"))
  "Non-nil when NEO should be verbose at startup.
Enabled by `emacs --debug-init' (which sets `init-file-debug') or by the
NEO_MODE=debug environment variable.  Drives `debug-on-error',
`message-log-max', and native-comp warning reporting; default is quiet.
Defined here, in the earliest-loaded core file, so both early-init.el and
`neo.el' can consult it.")

(defconst neo/default-emacs-instance-name "neo"
  "Default Neo Emacs instance name.")

(defvar neo/--emacs-instance-name-cache nil
  "Cached result of `neo/get-emacs-instance-name`.")

(defun neo/get-emacs-instance-name ()
  "Return the Emacs instance name from `--name`, or fallback to Neo's default.

Reads from /proc/self/cmdline if needed (Linux-only)."
  (or neo/--emacs-instance-name-cache
      (setq neo/--emacs-instance-name-cache
            (or
             ;; Try $EMACS_NAME for compatibility or scripting
             (getenv "EMACS_NAME")
             ;; Try /proc/self/cmdline for both -name (single-dash X flag) and --name
             (when (and (eq system-type 'gnu/linux)
                        (file-readable-p "/proc/self/cmdline"))
               (with-temp-buffer
                 (insert-file-contents-literally "/proc/self/cmdline")
                 (let* ((args (split-string (buffer-string) "\0" t))
                        (index (or (cl-position "--name" args :test #'string=)
                                   (cl-position "-name" args :test #'string=))))
                   (when index
                     (nth (1+ index) args)))))
             ;; Fallback
             neo/default-emacs-instance-name))))

(defun neo/default-emacs-instance-p ()
  "Return non-nil when the current instance uses Neo's default name."
  (string= (neo/get-emacs-instance-name) neo/default-emacs-instance-name))

(defun neo/nondefault-emacs-instance-p ()
  "Return non-nil when the current instance does not use Neo's default name."
  (not (neo/default-emacs-instance-p)))

(defconst neo--customize-disabled-message
  "Neo disables Customize persistence; edit configuration in code instead.")

(defun neo--custom-save-all-disabled (&rest _args)
  "Prevent Customize from writing persistent state in Neo."
  (if (called-interactively-p 'interactive)
      (user-error "%s" neo--customize-disabled-message)
    (message "%s" neo--customize-disabled-message)
    nil))

(defun neo/disable-customize-persistence ()
  "Disable Customize persistence for Neo."
  (setq custom-file null-device)
  (with-eval-after-load 'cus-edit
    (unless (advice-member-p #'neo--custom-save-all-disabled 'custom-save-all)
      (advice-add 'custom-save-all :override #'neo--custom-save-all-disabled))))

(defun neo/load-file (absolute-path &optional failure-ok)
  "Load the file at ABSOLUTE-PATH.

On success, return t.

On failure (file missing, unreadable, or LOAD errors):
- If FAILURE-OK is nil (default), signal an error.
- If FAILURE-OK is non-nil, return nil. Missing optional files stay silent,
  while actual LOAD errors are still reported.

This wrapper gives consistent behavior for all load failures without
distinguishing between missing and unreadable files."
  (unless (and (stringp absolute-path)
               (file-name-absolute-p absolute-path))
    (error "neo/load-file: ABSOLUTE-PATH must be an absolute filename (got %S)"
           absolute-path))
  (let ((load-error nil)
        (readable-p (file-readable-p absolute-path)))
    (condition-case err
        (when readable-p
          (load absolute-path nil 'nomessage)
          t)
      (error
       (setq load-error (error-message-string err))
       nil))
    (cond
     (load-error
      (if failure-ok
          (message "neo: error loading %s: %s" absolute-path load-error)
        (error "neo: error loading %s: %s" absolute-path load-error))
      nil)
     (readable-p t)
     (failure-ok nil)
     (t (error "neo: failed to load %s" absolute-path)))))

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
    (neo/load-file absolute-path failure-ok)))

(defun neo/data-directory ()
  "Return the XDG data directory for the current Neo instance."
  (expand-file-name (neo/get-emacs-instance-name)
                    (or (getenv "XDG_DATA_HOME")
                        "~/.local/share")))

(defun neo/data-file-path (filename)
  (expand-file-name filename (neo/data-directory)))

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

(defun neo/full-monty (_button)
  "Actually enable full monty mode and restart Emacs."
  (require 'neo-config)
  (neo/set-config "enabled-extensions" "(\"neo:full-monty\")")
  (neo/set-config "pretend-new-user" "nil")
  (neo/set-config "paraphenalia-config" "()")
  (if (fboundp 'restart-emacs)
      (restart-emacs)
    (save-buffers-kill-emacs)))

(defun neo/start-configuration (_button)
  "Boot with only the extension manager enabled, and land on it.

Persists `enabled-extensions' as just `(\"neo:extension-manager\")' -- that
extension has no `:requires' of its own, so it loads standalone -- and sets a
one-shot `\"launch-extensions-manager-on-startup\"' flag that
`neo/manager--maybe-launch-on-startup' (in the extension-manager extension)
consumes on the next boot to open the manager instead of a blank buffer."
  (require 'neo-config)
  (neo/set-config "enabled-extensions" "(\"neo:extension-manager\")")
  (neo/set-config "pretend-new-user" "nil")
  (neo/set-config "paraphenalia-config" "()")
  (neo/set-config "launch-extensions-manager-on-startup" "t")
  (if (fboundp 'restart-emacs)
      (restart-emacs)
    (save-buffers-kill-emacs)))

;; (defun neo--make-image-label (filename &optional scale)
;;   "Create a display label from FILENAME in the NEO assets directory.
;; Optional SCALE defaults to 1.0."
;;   (let ((img (create-image (expand-file-name filename neo--core-dir) nil nil 
;;                            :scale (or scale 1.0) 
;;                            :ascent 'center)))
;;     (propertize " " 'display img 'rear-nonsticky t)))

;; (defvar neo--intro "
;; The Red Pill: Full NEO initiation. Unleash a YOLO-tuned, monorepo-shredding environment where the Elisp is hot and the constraints are non-existent.\n\nThe Blue Pill: Return to Vanilla Emacs. A safe, beige, and blissfully \"productive\" purgatory where nothing exciting ever happens. You can still install add-ons manually, but you’ll be doing it in a world of lukewarm water and default keybindings.
;; ")

;; (defun neo/full-monty (_button)
;;   "Ask for confirmation before going full monty."
;;   (let ((buf (get-buffer-create "*Neo YOLO*"))
;;         (image-file (expand-file-name "yolo.png" neo--core-dir))
;;         (wconf (current-window-configuration)))
;;     (with-current-buffer buf
;;       (erase-buffer)
;;       (setq cursor-type nil)
;;       (insert-button (neo--make-image-label "red-pill128.png" .8) ;"[ RED PILL ]"
;; 		     'action #'neo/full-monty-confirm
;; 		     'follow-link t
;; 		     'face 'default      ; Removes the underline
;;                      'mouse-face nil     ; Removes the hover highlighting
;; 		     'help-echo "Wake up, Neo.")
;;       (when (file-exists-p image-file)
;;         (insert-image (create-image image-file)))
;;       (insert-button (neo--make-image-label "blue-pill128.png" .5) ;"[ BLUE PILL ]"
;; 		     'action (lambda (b)
;;                                (let ((saved-wconf (button-get b 'wconf)))
;; 				 (kill-buffer (current-buffer))
;; 				 (when saved-wconf (set-window-configuration saved-wconf))))
;; 		     'face 'default      ; Removes the underline
;;                      'mouse-face nil     ; Removes the hover highlighting
;; 		     'follow-link t
;; 		     'wconf wconf
;; 		     'help-echo "Back to sleep.")
;;       (insert "\n\n")
;;       (insert neo--intro)
;;       (switch-to-buffer buf)
;;       (delete-other-windows))))

(defun neo/fancy-splash--replace-args (orig-fun &rest args)
  "Replace arguments to `fancy-splash-insert` if the second argument is the \"To start\" string."
  (if (and (> (length args) 1)
           (string-match-p "To start" (nth 2 args)))
      (let ((separator (concat (make-string 60 ?─) "\n")))
        ;; Replace with your custom args
        (apply orig-fun
               `("\n" :face default ,separator
                 :face (:inherit variable-pitch :weight bold) "Welcome to Neo!\t"
                 :link ("Start configuration" ,#'neo/start-configuration "Help" (:weight bold))
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
                     'action #'neo/start-configuration
                     'follow-link t)
      (insert "    ")
      (insert-button "Don't do this 😜"
                     'action #'neo/full-monty
                     'follow-link t)
      (insert "\n" separator "\n"))))

(advice-add 'fancy-splash-insert :around #'neo/fancy-splash--replace-args)
(advice-add 'normal-splash-screen :after #'neo/normal-splash-screen-extra)


;;; Frame-size floor --------------------------------------------------------
;; Guarantee frames come up at a decent, splittable size even when no geometry
;; was saved (a fresh instance) or a stale/implausibly small one was restored
;; (e.g. a child/pop-up frame captured at exit).  See `neo/ensure-frame-size-floor'.

(defvar neo/default-frame-width 140
  "Default initial frame width in columns when no sane size is restored.")

(defvar neo/default-frame-height 42
  "Default initial frame height in rows when no sane size is restored.")

(defvar neo/minimum-frame-pixel-width 500
  "Restored frames narrower than this many pixels are treated as bogus
\(e.g. a child/pop-up frame captured at exit) and reset to the default.")

(defvar neo/minimum-frame-pixel-height 350
  "Restored frames shorter than this many pixels are treated as bogus and
reset to the default.")

(defvar neo/minimum-frame-cols 24
  "Restored frames narrower than this many columns are treated as bogus.")

(defvar neo/minimum-frame-rows 10
  "Restored frames shorter than this many rows are treated as bogus.")

(defvar neo--restored-frame-geometry nil
  "Frame size/position restored from disk at startup.
Captured by `neo/ensure-frame-size-floor' before the frame alists are used,
and applied explicitly by `neo/apply-restored-frame-geometry' as a safety net
for toolkit builds that ignore restored geometry at frame creation.")

(defun neo--frame-dimension-too-small-p (entry min-px min-chars)
  "Return non-nil when frame-alist ENTRY describes an implausibly small size.
ENTRY is a (width . V) or (height . V) cons.  Handles a plain character
count and the (text-pixels . PX) form written by
`neo/save-initial-frame-properties'."
  (when entry
    (let ((v (cdr entry)))
      (cond
       ((and (consp v) (eq (car v) 'text-pixels)) (< (cdr v) min-px))
       ((integerp v) (< v min-chars))
       (t nil)))))

(defun neo--frame-alist-ensure-size (alist-sym)
  "Guarantee ALIST-SYM has a decent, splittable width and height.
Missing dimensions get char-based defaults; implausibly small restored
dimensions are replaced by them.  Return non-nil if anything was repaired."
  (let ((alist (symbol-value alist-sym))
        (repaired nil))
    (dolist (dim '(width height))
      (let* ((entry (assq dim alist))
             (default  (if (eq dim 'width) neo/default-frame-width neo/default-frame-height))
             (min-px   (if (eq dim 'width) neo/minimum-frame-pixel-width neo/minimum-frame-pixel-height))
             (min-chars (if (eq dim 'width) neo/minimum-frame-cols neo/minimum-frame-rows)))
        (cond
         ((null entry)
          (setq alist (cons (cons dim default) alist) repaired t))
         ((neo--frame-dimension-too-small-p entry min-px min-chars)
          (setcdr entry default)
          (setq repaired t)))))
    (set alist-sym alist)
    repaired))

(defun neo/ensure-frame-size-floor ()
  "Ensure NEO frames come up at a sane, splittable size.
Floors both `initial-frame-alist' and `default-frame-alist', captures the
restored geometry into `neo--restored-frame-geometry', and mirrors the
restored SIZE into `default-frame-alist'.

The mirror matters: on this toolkit build the initial frame is created from
`default-frame-alist' and the geometry restored into `initial-frame-alist' is
ignored, so the size must live in default-frame-alist too.  Position is NOT
copied there (that would stack every new frame at one spot); it is left to the
window manager, which also stops the saved position from drifting."
  (neo--frame-alist-ensure-size 'initial-frame-alist)
  ;; Never restore POSITION: on a reparenting window manager the set/report
  ;; round-trip drifts, so a restored position makes the frame "walk" across
  ;; launches.  Drop any saved left/top (also from older files) so the window
  ;; manager places the frame.
  (setq initial-frame-alist
        (assq-delete-all 'left (assq-delete-all 'top initial-frame-alist)))
  (setq neo--restored-frame-geometry
        (seq-filter (lambda (cell) (memq (car-safe cell) '(width height)))
                    initial-frame-alist))
  (let ((w (assq 'width initial-frame-alist))
        (h (assq 'height initial-frame-alist)))
    (when w (setf (alist-get 'width default-frame-alist) (cdr w)))
    (when h (setf (alist-get 'height default-frame-alist) (cdr h))))
  (neo--frame-alist-ensure-size 'default-frame-alist))

;;; Post-creation frame repair -----------------------------------------------
;; The floor above only shapes the alists consulted at frame *creation*; on
;; this toolkit build the initial frame is sometimes created collapsed anyway
;; (see `neo--repair-collapsed-frame'). These functions repair that after the
;; fact and persist geometry for the next launch. They live here (not in the
;; neo:ui extension) so they run on every boot, including the first-run splash
;; screen and the extension-manager-only restart from `neo/start-configuration'
;; -- neither of which loads neo:ui or any other extension.

;; TODO maybe we could do something interesting for terminals with window-system-default-frame-alist.
(defun neo--frame-geometry-alist (frame)
  "Return an alist describing FRAME's position and size for restoration.
Size is saved in CHARACTER columns/rows (honored by `default-frame-alist' at
frame creation on this build).  Position is saved for reference only and is
not restored (the window manager places the frame).  When FRAME is maximized
or fullscreen, that state is saved instead of an explicit geometry."
  (let ((fullscreen (frame-parameter frame 'fullscreen)))
    (if fullscreen
        (list (cons 'fullscreen fullscreen))
      ;; Save SIZE ONLY, in CHARACTER columns/rows (honored by
      ;; `default-frame-alist' at frame creation on this build).  Position is
      ;; deliberately NOT persisted or restored: on a reparenting window manager
      ;; the set/report position round-trip drifts by a constant, so restoring
      ;; it makes the frame "walk" across launches.  We let the window manager
      ;; place the frame instead.
      (list (cons 'width (frame-width frame))
            (cons 'height (frame-height frame))))))

(defun neo--frame-alist-string (var alist)
  "Return Elisp setting VAR to the quoted ALIST, one entry per line."
  (concat (format "(setq %s\n      '(" var)
          (mapconcat (lambda (cell) (format "%S" cell)) alist "\n        ")
          "))\n"))

(defun neo--frame-suitable-for-save-p (frame)
  "Return non-nil when FRAME's geometry is worth persisting.
Excludes non-graphic frames, tooltips, child/pop-up frames (posframe,
corfu, …), and frames too small to be usefully split — any of which would
otherwise clobber the saved geometry with an unusable size."
  (and (frame-live-p frame)
       (display-graphic-p frame)
       (frame-visible-p frame)
       (not (frame-parameter frame 'parent-frame))   ; not a child frame
       (not (frame-parameter frame 'tooltip))
       (>= (frame-text-width frame) neo/minimum-frame-pixel-width)
       (>= (frame-text-height frame) neo/minimum-frame-pixel-height)))

(defun neo--frame-to-save ()
  "Return the frame whose geometry should be persisted, or nil if none.
Prefer the selected frame; otherwise fall back to the largest suitable
frame.  Returning nil means \"do not overwrite the saved geometry\", so a
session with only unsuitable frames leaves the last good geometry intact."
  (if (neo--frame-suitable-for-save-p (selected-frame))
      (selected-frame)
    (car (sort (seq-filter #'neo--frame-suitable-for-save-p (frame-list))
               (lambda (a b)
                 (> (* (frame-text-width a) (frame-text-height a))
                    (* (frame-text-width b) (frame-text-height b))))))))

(defun neo/save-initial-frame-properties ()
  "Save a suitable frame's geometry, font, and default face colors.
The result is written to `initial-frame-properties.el' in the current
profile so early-init.el restores the frame on the next launch.  Both
position and size (in pixels) are preserved, along with any maximized or
fullscreen state.  This runs on `kill-emacs-hook'.  Only a real, visible,
top-level graphic frame that is large enough is saved (see
`neo--frame-suitable-for-save-p'); if none exists the previous saved
geometry is left untouched rather than overwritten with a bogus size."
  (interactive)
  (when-let* ((frame (neo--frame-to-save)))
    (let* ((font (frame-parameter frame 'font))
           (look `((font . ,font)
                   (internal-border-width . 0)
                   (undecorated . nil)))
           (bg (face-background 'default nil t))
           (fg (face-foreground 'default nil t))
           (family (face-attribute 'default :family nil 'default))
           (font-height (face-attribute 'default :height nil 'default))
           (file (neo/config-file-path "initial-frame-properties.el")))
      (make-directory (file-name-directory file) t)
      (with-temp-file file
        (insert ";; Auto-generated by neo/save-initial-frame-properties\n")
        (insert ";; Restores frame geometry, font, and default face colors.\n\n")
        ;; Only the initial frame is pinned to the saved geometry; later
        ;; frames inherit just the look so they do not stack at one spot.
        (insert (neo--frame-alist-string "initial-frame-alist"
                                         (append (neo--frame-geometry-alist frame)
                                                 look)))
        (insert "\n")
        (insert (neo--frame-alist-string "default-frame-alist" look))
        (insert "\n")
        (insert (format "(set-face-attribute 'default nil :background %S :foreground %S :family %S :height %d)\n"
                        bg fg family font-height))))))

(add-hook 'kill-emacs-hook #'neo/save-initial-frame-properties)

(defun neo/ensure-frame-onscreen-and-usable (&optional frame)
  "Force FRAME (default selected) to a usable size and on-screen position.
Safety net for a frame that ends up too small to split into windows — e.g.
restored geometry the toolkit did not honor, or a window-manager clamp.
Sizes the frame up to at least the NEO default, never beyond the monitor
work area, and nudges it fully on-screen.  Child/pop-up frames (posframe,
corfu, …) are left untouched.  Idempotent: a frame already large enough and
on-screen is not touched."
  (setq frame (or frame (selected-frame)))
  (when (and (frame-live-p frame)
             (display-graphic-p frame)
             (not (frame-parameter frame 'parent-frame)))
    (when-let* ((wa (frame-monitor-workarea frame))
                (wx (nth 0 wa)) (wy (nth 1 wa)) (ww (nth 2 wa)) (wh (nth 3 wa)))
      (let* ((cw (max 1 (frame-char-width frame)))
             (chh (max 1 (frame-char-height frame)))
             (max-cols (max neo/minimum-frame-cols (/ ww cw)))
             (max-rows (max neo/minimum-frame-rows (/ wh chh)))
             (cols (frame-width frame))
             (rows (frame-height frame))
             (want-cols (min max-cols (max cols neo/default-frame-width)))
             (want-rows (min max-rows (max rows neo/default-frame-height))))
        (when (or (/= want-cols cols) (/= want-rows rows))
          (set-frame-size frame want-cols want-rows))
        ;; Reposition ONLY when the frame is off-screen by more than a small
        ;; margin.  Nudging an already-visible frame every launch is what makes
        ;; the position "jump around", so leave a visible frame exactly where the
        ;; window manager put it.  Also skip entirely while the frame still
        ;; reports the toolkit's known ~200x200px transient collapse signature
        ;; (see `neo--repair-collapsed-frame'): pixel geometry read during that
        ;; transient is meaningless, and computing an offset from it is what
        ;; drove the frame into the bottom-right corner instead of merely
        ;; leaving it alone until the real size settles.
        (let* ((pw (frame-pixel-width frame))
               (ph (frame-pixel-height frame)))
          (unless (or (< pw neo/minimum-frame-pixel-width)
                      (< ph neo/minimum-frame-pixel-height))
            (let* ((pos (frame-position frame))
                   (fx (car pos)) (fy (cdr pos))
                   (margin 16)
                   (nx (cond ((< fx (- wx margin)) wx)
                             ((> (+ fx pw) (+ wx ww margin)) (max wx (- (+ wx ww) pw)))
                             (t fx)))
                   (ny (cond ((< fy (- wy margin)) wy)
                             ((> (+ fy ph) (+ wy wh margin)) (max wy (- (+ wy wh) ph)))
                             (t fy))))
              (when (or (/= nx fx) (/= ny fy))
                (set-frame-position frame nx ny)))))))))

(defun neo--repair-collapsed-frame (&optional frame)
  "Resize FRAME back to its intended size if the toolkit created it collapsed.
About 1 launch in 4, this Emacs build creates the initial frame at ~200x200px
(roughly 20x11 chars) instead of the requested size, and it never
self-corrects (see ~/repro for the analysis).  ONLY when the frame is that far
too small do we force the restored size back — the character size, the legacy
`(text-pixels . N)' size, or the NEO default when nothing was saved.  This
NEVER repositions the frame, so running it on retry timers cannot make the
frame walk."
  (setq frame (or frame (selected-frame)))
  (when (and (frame-live-p frame)
             (display-graphic-p frame)
             (not (frame-parameter frame 'parent-frame))
             (or (< (frame-width frame) neo/minimum-frame-cols)
                 (< (frame-height frame) neo/minimum-frame-rows)))
    (let* ((geom (bound-and-true-p neo--restored-frame-geometry))
           (w (cdr (assq 'width geom)))
           (h (cdr (assq 'height geom))))
      (cond
       ((and (consp w) (eq (car w) 'text-pixels)
             (consp h) (eq (car h) 'text-pixels))
        (set-frame-size frame (cdr w) (cdr h) t))
       ((and (integerp w) (integerp h))
        (set-frame-size frame w h))
       (t (set-frame-size frame neo/default-frame-width neo/default-frame-height))))))

(defun neo/apply-restored-frame-geometry (&optional frame)
  "Repair a collapsed FRAME then apply the on-screen clamp and usable floor.
Repositioning happens only here (once at startup / per new frame), never on
the retry timers.  Also arms the reactive `window-size-change-functions'
repair (see comment below) -- deliberately NOT armed any earlier than this,
so it cannot see the toolkit's own noisy intermediate sizes while the initial
frame is still being realized/mapped, which could otherwise misfire and
visibly yank the frame mid-realization."
  (neo--repair-collapsed-frame frame)
  (neo/ensure-frame-onscreen-and-usable frame)
  (add-hook 'window-size-change-functions #'neo--repair-collapsed-frame))

(defun neo--schedule-frame-collapse-retries ()
  "Schedule a few short retries of the resize-only collapse repair.
Anchored to `emacs-startup-hook' rather than file load time: this file loads
from `early-init.el', well before the initial frame is even created, so
delays counted from then no longer reliably bracket the toolkit's collapse,
which can manifest asynchronously well after startup finishes.  These are a
fallback for `window-size-change-functions', which is the primary,
timing-independent catch once armed."
  (dolist (delay '(0.2 0.6 1.2 2.5 5.0))
    (run-with-timer delay nil #'neo--repair-collapsed-frame)))

;; Repair a collapsed frame at startup, on a few short retry timers, and
;; (from that point on) reactively whenever Emacs notices a frame/window size
;; change (the collapse can appear late and never self-corrects, and the
;; toolkit's own resize event is a more reliable trigger than any fixed
;; delay).  The `window-size-change-functions' hook is armed inside
;; `neo/apply-restored-frame-geometry' itself -- not registered here at load
;; time -- so it only starts watching once Emacs's own startup has completed
;; and the initial frame has already been realized once; registering it any
;; earlier would let it react to the toolkit's transient in-progress sizes
;; while the window manager is still mapping the very first frame.  The
;; retries and the reactive hook call the resize-ONLY repair — they never
;; reposition — so they cannot make the frame walk.  The on-screen clamp/floor
;; runs once at startup and for each new frame.
(add-hook 'emacs-startup-hook #'neo/apply-restored-frame-geometry)
(add-hook 'emacs-startup-hook #'neo--schedule-frame-collapse-retries)

(defun neo--defer-ensure-frame-onscreen-and-usable (frame)
  "Run `neo/ensure-frame-onscreen-and-usable' on FRAME after this tick.
`after-make-frame-functions' runs synchronously in the same beat as frame
creation, before the window manager/toolkit has finished laying the frame
out -- on this GTK3 build that is exactly when pixel geometry can still read
the transient ~200x200px collapse signature.  Deferring by one command loop
iteration lets that settle first instead of computing a resize/reposition
from bogus numbers, which is what visibly yanked the frame to the
bottom-right corner during the very first (splash) boot once this repair
started running on every frame instead of only when neo:ui was loaded."
  (run-with-timer 0 nil #'neo/ensure-frame-onscreen-and-usable frame))

(add-hook 'after-make-frame-functions #'neo--defer-ensure-frame-onscreen-and-usable)

(provide 'neo-early-init-utils)
