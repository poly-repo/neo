;;; neo-window.el --- Window management utilities for Neo -*- lexical-binding: t -*--

(require 'cl-lib)

(defun neo--display-and-select (buffer alist)
  "Display BUFFER in a side window and select it.
ALIST is the display-buffer alist."
  (let ((window (display-buffer-in-side-window buffer alist)))
    (when window
      (select-window window)
      window)))

(cl-defun neo/make-side-display (&key side size slot persistent)
  "Create a display-buffer action for a dedicated side window.

SIDE is one of: left, right, top, bottom.
SIZE means width for left/right, height for top/bottom.
SLOT controls ordering among side windows.
PERSISTENT, when non-nil, marks this side window as reusable: its
buffer is resurrected by `neo/toggle-side-window' after its window is
dismissed instead of falling back to the side's registered default
action.

Returns (FUNCTION . ALIST) suitable for `display-buffer-alist`."
  (let* ((side (or side 'right))
         (slot (or slot 0))
         (horizontal (memq side '(left right))))
    `((neo--display-and-select)
      (side . ,side)
      (slot . ,slot)
      ,@(when size
          (if horizontal
              `((window-width . ,size)
                (preserve-size . (t . nil)))
            `((window-height . ,size)
              (preserve-size . (nil . t)))))
      (dedicated . t)
      (inhibit-same-window . t)
      (persistent . ,persistent)
      (window-parameters
       (mode-line-format . none)))))

(defun neo/side-window (&rest args)
  "Register a side window configuration.
ARGS can include:
:regex - A regular expression matching buffer names.
:mode - A major mode symbol.
:include-derived - If non-nil and :mode is specified, match derived modes.
:side - The side of the frame ('left, 'right, 'top, 'bottom).
:size - The size of the window.
:slot - The slot for the side window.
:persistent - If non-nil, `neo/toggle-side-window' resurrects this
buffer after its window is dismissed instead of falling back to the
side's registered default action. Intended for windows the user
explicitly toggles open and closed (e.g. Treemacs, eshell), as
opposed to transient views (Help, Info, compilation output) that
should release their slot once dismissed.

Examples:
(neo/side-window :regex \"^\\*gemini.*\\*$\" :side 'right :size 80)
(neo/side-window :mode 'help-mode :include-derived t :side 'right :size 80)"
  (let ((regex (plist-get args :regex))
        (mode (plist-get args :mode))
        (include-derived (plist-get args :include-derived))
        (side (plist-get args :side))
        (size (plist-get args :size))
        (slot (plist-get args :slot))
        (persistent (plist-get args :persistent)))
    (cond
     (regex
      (add-to-list 'display-buffer-alist
                   (cons regex
                         (neo/make-side-display :side side :size size :slot slot
                                                 :persistent persistent))))
     (mode
      (add-to-list 'display-buffer-alist
                   (cons (lambda (buf _action)
                           (with-current-buffer buf
                             (if include-derived
                                 (derived-mode-p mode)
                               (eq major-mode mode))))
                         (neo/make-side-display :side side :size size :slot slot
                                                 :persistent persistent))))
     (t
      (error "neo/side-window: Must specify :regex or :mode")))))

(defun neo/buffer-match-p (condition buffer)
  "Return non-nil if CONDITION matches BUFFER."
  (cond
   ((stringp condition)
    (string-match-p condition (buffer-name buffer)))
   ((functionp condition)
    (funcall condition buffer nil))
   (t nil)))

(defun neo/buffer-side-window-alist (buffer)
  "Return BUFFER's side-window display-buffer-alist ALIST, or nil.
Only returns the ALIST when BUFFER's matching `display-buffer-alist'
entry targets a side window (via `display-buffer-in-side-window' or
`neo--display-and-select')."
  (let ((match (cl-find-if (lambda (entry)
                             (neo/buffer-match-p (car entry) buffer))
                           display-buffer-alist)))
    (when match
      (let* ((action (cdr match))
             (functions (if (listp (car action))
                            (car action)
                          (list (car action))))
             (alist (cdr action)))
        (when (or (memq 'display-buffer-in-side-window functions)
                  (memq 'neo--display-and-select functions))
          alist)))))

(defun neo/buffer-targets-side-window-p (buffer &optional side)
  "Return non-nil if BUFFER is configured to be displayed in a side window.
If SIDE is non-nil, checks if it targets that specific SIDE."
  (when-let* ((alist (neo/buffer-side-window-alist buffer)))
    (if side
        (eq side (alist-get 'side alist))
      t)))

(defun neo/buffer-persistent-side-window-p (buffer)
  "Return non-nil if BUFFER was registered with `neo/side-window' :persistent t."
  (when-let* ((alist (neo/buffer-side-window-alist buffer)))
    (alist-get 'persistent alist)))

(defun neo/get-side-window-buffers (&optional side persistent-only)
  "Return a list of all live buffers that target a side window.
If SIDE is non-nil (one of 'left, 'right, 'top, 'bottom), return only
buffers targeting that side. If PERSISTENT-ONLY is non-nil, only
include buffers registered with `neo/side-window' :persistent t."
  (cl-loop for buf in (buffer-list)
           when (and (neo/buffer-targets-side-window-p buf side)
                     (or (not persistent-only)
                         (neo/buffer-persistent-side-window-p buf)))
           collect buf))

(provide 'neo-window)