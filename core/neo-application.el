;;; neo-application.el --- Application management for Neo -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'subr-x)

(defgroup neo-application nil
  "Settings for Neo applications."
  :group 'neo
  :prefix "neo-application-")

(defcustom neo-application-default-quit-keys '("q")
  "Keys that leave a Neo application and restore the previous perspective.
Each entry is a `kbd' string.  Applications inherit this list unless they
pass an explicit `:quit-keys' to `neo/application'; a value of nil leaves
the native quit behavior of the application untouched."
  :type '(repeat string)
  :group 'neo-application)

(defvar neo/applications-map (make-sparse-keymap)
  "Keymap for Neo applications.")

(global-set-key (kbd "M-a") neo/applications-map)

(cl-defstruct (neo/application
               (:copier nil))
  "Represents a single Neo application."
  name
  setup
  teardown
  binding
  command
  quit-keys)

(defvar neo--applications (make-hash-table :test #'equal)
  "Hash table mapping application names to `neo/application` instances.")

(defun neo/applications ()
  "Return registered `neo/application' instances, sorted by name."
  (sort (hash-table-values neo--applications)
        (lambda (a b)
          (string< (neo/application-name a) (neo/application-name b)))))

(defun neo/application-names ()
  "Return the sorted list of registered Neo application names."
  (mapcar #'neo/application-name (neo/applications)))

(defvar neo--application-perspective-stack nil
  "Stack of perspectives to return to as Neo applications are exited.
Each `neo/application' entry pushes the perspective it switched away
from, regardless of whether that perspective is itself another
application; quitting pops and restores it.  This makes nested
application launches (e.g. opening one app from within another) return
to the correct predecessor at each step, rather than only ever being
able to restore a single remembered \"user\" perspective.")

(defvar neo-application-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<S-up> <S-up>") #'neo/leave-current-application)
    (define-key map (kbd "<S-up> <up>") #'neo/leave-current-application)
    map)
  "Keymap for `neo-application-mode`.")

(define-minor-mode neo-application-mode
  "Minor mode active in Neo applications."
  :init-value nil
  :lighter " NeoApp"
  :keymap neo-application-mode-map)

(defvar neo/application-forbidden-buffers nil
  "List of buffer names that should be removed from the application perspective.")

(defun neo/application--post-setup-cleanup (app-name)
  "Cleanup windows showing scratch or messages after application setup.
APP-NAME is the name of the application (without the 'App:' prefix)."
  (let ((scratch-buffer-name (format "*scratch* (App:%s)" app-name))
        (global-scratch "*scratch*")
        (messages-buffer-name "*Messages*")
        (warnings-buffer-name "*Warnings*"))
    ;; Remove forbidden buffers from the perspective
    (dolist (buf-name neo/application-forbidden-buffers)
      (when-let* ((buf (get-buffer buf-name)))
        (persp-remove-buffer buf)))

    ;; Delete windows showing scratch buffers or *Messages* or *Warnings*
    (dolist (win (window-list))
      (let ((buf-name (buffer-name (window-buffer win))))
        (when (or (string= buf-name scratch-buffer-name)
                  (string= buf-name global-scratch)
                  (string= buf-name messages-buffer-name)
                  (string= buf-name warnings-buffer-name))
          (ignore-errors (delete-window win)))))))

(defun neo/application--active-in-frame-p ()
  "Return non-nil when some window in the selected frame shows a buffer
with `neo-application-mode' enabled.
Relies on `persp-mode' swapping the frame's entire window configuration
on `persp-switch', so a buffer from a different (non-current) Neo
application's perspective is never visible here even though the mode
itself is not app-specific."
  (cl-some (lambda (win)
             (buffer-local-value 'neo-application-mode (window-buffer win)))
           (window-list)))

(defun neo/leave-current-application ()
  "Leave the current Neo application and run its teardown."
  (interactive)
  (require 'perspective)
  (let ((current (persp-current-name)))
    (unless (string-prefix-p "App:" current)
      (user-error "Not in a Neo application perspective"))
    (let* ((app-name (substring current 4))
           (app (gethash app-name neo--applications)))
      (if app
          (progn
            (neo-application-mode -1)
            (eval (neo/application-teardown app)))
        (user-error "Unknown application: %s" app-name)))))

(defun neo/application--install-quit-keys (keys)
  "Bind each key in KEYS to `neo/leave-current-application' in this buffer.
KEYS is a list of `kbd' strings.  The bindings live in a buffer-local
keymap that overrides `neo-application-mode' for the current buffer only,
so they take precedence over the application's own major-mode bindings
without mutating any shared keymap.  The map inherits
`neo-application-mode-map', preserving its default bindings.  With KEYS
nil nothing is installed and the native quit behavior is left untouched."
  (when keys
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map neo-application-mode-map)
      (dolist (key keys)
        (define-key map (kbd key) #'neo/leave-current-application))
      (setq-local minor-mode-overriding-map-alist
                  (cons (cons 'neo-application-mode map)
                        (assq-delete-all
                         'neo-application-mode
                         (copy-alist minor-mode-overriding-map-alist)))))))

(defmacro neo/application (name &rest args)
  "Register a new Neo application.

NAME is the name of the application (string).

Keywords arguments:
:setup     - Form to run to set up the application.
:teardown  - Form to run to tear down the application (optional).
:bind      - Keybinding key (string) relative to `neo/applications-map`.
:quit-keys - Unquoted list of `kbd' strings (e.g. (\"q\")) that leave the
             application and restore the previous perspective.  Defaults
             to `neo-application-default-quit-keys'; pass nil to keep the
             native quit behavior of the application."
  (declare (indent 1))
  (let* ((setup (plist-get args :setup))
         (teardown (plist-get args :teardown))
         (binding (plist-get args :bind))
         ;; Resolve the default at runtime, not expansion time: the value
         ;; of `neo-application-default-quit-keys' is not available while
         ;; byte-compiling the files that register applications.
         (quit-keys-form (if (plist-member args :quit-keys)
                             (list 'quote (plist-get args :quit-keys))
                           'neo-application-default-quit-keys))
         (cmd-name (intern (format "neo/app-%s" (replace-regexp-in-string " " "-" (downcase name)))))
         (augmented-teardown
          `(progn
             ,teardown
             (when-let* ((target (pop neo--application-perspective-stack)))
               (when (member target (persp-names))
                 (persp-switch target))))))
    `(progn
       (defun ,cmd-name ()
         ,(format "Switch to %s application." name)
         (interactive)
         (require 'perspective)
         (unless (bound-and-true-p persp-mode)
           (persp-mode 1))
         (let* ((current-persp (persp-current-name))
                (app-persp-name (format "App:%s" ,name))
                (already-there (string= current-persp app-persp-name)))
           (cond
            ((and already-there (neo/application--active-in-frame-p))
             (message "Already in application %s" ,name))
            (t
             ;; Always push, even when coming from another application: this
             ;; is what lets nested launches (e.g. opening Neo Extensions
             ;; from within the Dashboard) unwind one step at a time instead
             ;; of only ever restoring a single remembered perspective. Skip
             ;; it entirely when we're already in the app's own perspective
             ;; but it's not alive (e.g. its buffer was killed with nothing
             ;; to restore to on a previous quit) -- we never left, so there
             ;; is nothing to push or switch to; just re-run setup in place.
             (unless already-there
               (push current-persp neo--application-perspective-stack)
               (persp-switch app-persp-name))
             ,setup
             (neo/application--post-setup-cleanup ,name)
             ;; `setup' may run inside `with-current-buffer' (e.g.
             ;; `elpaca-manager'), so the current buffer can revert once it
             ;; returns even though the application's window is selected.
             ;; Target the selected window's buffer so the minor mode and
             ;; quit keys land on the application's actual buffer.
             (with-current-buffer (window-buffer (selected-window))
               (neo-application-mode 1)
               (neo/application--install-quit-keys ,quit-keys-form))))))
       ,@(when binding
           `((define-key neo/applications-map (kbd ,binding) #',cmd-name)))
       (puthash ,name
                (make-neo/application
                 :name ,name
                 :setup ',setup
                 :teardown ',augmented-teardown
                 :binding ,binding
                 :command ',cmd-name
                 :quit-keys ,quit-keys-form)
                neo--applications))))

(neo/application "Calc"
  :setup (calc)
  :bind "c")

(neo/application "Dashboard"
  :setup (neo/dashboard)
  :bind "d"
  ;; The dashboard owns its own "q" binding (`neo/dashboard-quit'), which
  ;; restores the perspective it displaced; leave it untouched.
  :quit-keys nil)

(provide 'neo-application)
