;;; emacs-snapshot-safe.el --- Safe Emacs state snapshot / diff / restore -*- lexical-binding: t; -*-

;; Usage:
;; 1. (setq my-snap-before (take-emacs-snapshot))
;; 2. ... load/test config ...
;; 3. (setq my-snap-after (take-emacs-snapshot))
;; 4. (setq my-diff (emacs-snapshot-diff-safe my-snap-before my-snap-after))
;; 5. Inspect `my-diff` and call relevant restore helpers.

;;;; Utilities

(defun snapshot--current-time-string ()
  (format-time-string "%Y%m%dT%H%M%S"))

(defun snapshot--safe-symbol-value (sym)
  "Return symbol-value of SYM safely (or the symbol '<<unreadable>> on error)."
  (condition-case _err
      (symbol-value sym)
    (error '<<unreadable>>)))

(defun snapshot--safe-symbol-function (sym)
  "Return symbol-function of SYM safely (or the symbol '<<unreadable>> on error)."
  (condition-case _err
      (symbol-function sym)
    (error '<<unreadable>>)))

(defun snapshot--timers ()
  "Return a list of active timers, using whichever symbol is available."
  (let ((sym (cond
              ((boundp 'timer-list) 'timer-list)
              ((boundp 'internal-timer-list) 'internal-timer-list)
              (t nil))))
    (when sym (condition-case _err (symbol-value sym) (error nil)))))

(defun snapshot--processes ()
  "Return the list of processes (copy)."
  (condition-case _err (copy-sequence (process-list)) (error nil)))

(defun snapshot--features ()
  "Return a copy of `features`."
  (condition-case _err (copy-sequence features) (error nil)))

(defun snapshot--load-history ()
  "Return a (shallow) copy of `load-history`."
  (condition-case _err (copy-tree load-history) (error nil)))

;;;; Snapshot collectors (safe)

(defun snapshot--all-function-cells ()
  "Return an alist (SYM . FUNCCELL) for symbols with function cells.
Does not stringify function objects (safe)."
  (let (out)
    (mapatoms
     (lambda (s)
       (when (fboundp s)
         (let ((fc (condition-case _err (symbol-function s) (error '<<unreadable>>))))
           (push (cons s fc) out)))))
    out))

(defun snapshot--variables-safe ()
  "Return an alist (SYM . VALUE) for bound variables, safely."
  (let (out)
    (mapatoms
     (lambda (s)
       (when (boundp s)
         (let ((val (condition-case _err (symbol-value s) (error '<<unreadable>>))))
           (push (cons s val) out)))))
    out))

(defun snapshot--faces-safe ()
  "Return list of face symbols (avoid heavy attribute inspection)."
  (condition-case _err (copy-sequence (face-list)) (error nil)))

(defun snapshot--global-keybindings ()
  "Return an alist of (KEYSTRING . BINDING) for the current global map.
KEYSTRING is produced with `key-description`. BINDING is the raw binding object
(not pretty-printed) to avoid autoload/printing side-effects."
  (let (out)
    (condition-case _err
        (progn
          (map-keymap
           (lambda (ev binding)
             (let ((ks (condition-case _ (key-description (vector ev)) (format "<%s>" ev))))
               (push (cons ks binding) out)))
           (current-global-map))
          out)
      (error nil))))

(defun snapshot--hook-contents ()
  "Return an alist (HOOK-SYM . COPY-OF-VALUE) for hook variables.
Only includes symbols whose name ends with \"-hook\" and whose value is a list."
  (let (out)
    (dolist (sym (apropos-internal ".*-hook$" 'boundp))
      (when (and (boundp sym)
                 (listp (symbol-value sym)))
        (push (cons sym (copy-sequence (symbol-value sym))) out)))
    out))

;;;; Top-level snapshot

(defun take-emacs-snapshot ()
  "Capture a snapshot of many parts of Emacs state (safe, low-side-effects).
Returns an alist with keys like :time :functions :variables :faces :global-keybindings
:hooks :timers :processes :features :load-history"
  (list
   (cons :time (snapshot--current-time-string))
   (cons :functions (snapshot--all-function-cells))
   (cons :variables (snapshot--variables-safe))
   (cons :faces (snapshot--faces-safe))
   (cons :global-keybindings (snapshot--global-keybindings))
   (cons :hooks (snapshot--hook-contents))
   (cons :timers (snapshot--timers))
   (cons :processes (snapshot--processes))
   (cons :features (snapshot--features))
   (cons :load-history (snapshot--load-history))))

;;;; Diff (safe)

(defun emacs-snapshot-diff-safe (snap-before snap-after)
  "Return a conservative plist describing differences between SNAP-BEFORE and SNAP-AFTER.
Only reports additions and conservative changes; avoids printing function objects."
  (let (diff)
    ;; functions: added or redefined (identity eq for function cell)
    (let* ((before (alist-get :functions snap-before))
           (after  (alist-get :functions snap-after))
           (bhash (make-hash-table :test #'eq))
           (ahash (make-hash-table :test #'eq)))
      (dolist (p before) (puthash (car p) (cdr p) bhash))
      (dolist (p after ) (puthash (car p) (cdr p) ahash))
      (let (added redefined)
        (maphash
         (lambda (sym func)
           (unless (gethash sym bhash) (push sym added))
           (when (and (gethash sym bhash) (not (eq (gethash sym bhash) func)))
             (push sym redefined)))
         ahash)
        (when added (push (cons :functions-added (nreverse added)) diff))
        (when redefined (push (cons :functions-redefined (nreverse redefined)) diff)))))
    ;; variables: added and changed (conservative: use equal where possible)
    (let* ((bvars (mapcar #'car (alist-get :variables snap-before)))
           (avars (mapcar #'car (alist-get :variables snap-after)))
           (new-vars (seq-difference avars bvars)))
      (when new-vars (push (cons :variables-added new-vars) diff))
      (let (changed)
        (dolist (v avars)
          (when (member v bvars)
            (condition-case _err
                (let ((bv (alist-get v (alist-get :variables snap-before)))
                      (av (alist-get v (alist-get :variables snap-after))))
                  (unless (or (eq bv av) (equal bv av)) (push v changed)))
              (error nil))))
        (when changed (push (cons :variables-changed (nreverse changed)) diff))))
    ;; faces: added (we only store names)
    (let* ((bf (alist-get :faces snap-before))
           (af (alist-get :faces snap-after))
           (bnames (mapcar #'identity bf))
           (anames (mapcar #'identity af))
           (fadded (seq-difference anames bnames)))
      (when fadded (push (cons :faces-added fadded) diff)))
    ;; global keys: new keystrings only
    (let* ((bkeys (mapcar #'car (alist-get :global-keybindings snap-before)))
           (akeys (mapcar #'car (alist-get :global-keybindings snap-after)))
           (added (seq-difference akeys bkeys)))
      (when added (push (cons :global-keys-added added) diff)))
    ;; hooks: new hook variables
    (let* ((bh (mapcar #'car (alist-get :hooks snap-before)))
           (ah (mapcar #'car (alist-get :hooks snap-after)))
           (new-hooks (seq-difference ah bh)))
      (when new-hooks (push (cons :hooks-added new-hooks) diff)))
    ;; features loaded
    (let* ((bfeat (alist-get :features snap-before))
           (afeat (alist-get :features snap-after))
           (new-features (seq-difference afeat bfeat)))
      (when new-features (push (cons :features-loaded new-features) diff)))
    ;; timers/processes: conservative (seq-difference)
    (let* ((bt (alist-get :timers snap-before))
           (at (alist-get :timers snap-after))
           (new-timers (when (and bt at) (seq-difference at bt))))
      (when (and new-timers (not (equal new-timers '()))) (push (cons :timers-added new-timers) diff)))
    (let* ((bp (alist-get :processes snap-before))
           (ap (alist-get :processes snap-after))
           (new-procs (when (and bp ap) (seq-difference ap bp))))
      (when (and new-procs (not (equal new-procs '()))) (push (cons :processes-started new-procs) diff)))
    (nreverse diff)))

;;;; Restore helpers (best-effort). Use with caution.

(defun restore-functions (sym-list before-snap)
  "Restore functions in SYM-LIST using definitions recorded in BEFORE-SNAP.
If a function did not exist before, it calls `fmakunbound` on the symbol."
  (let ((before (alist-get :functions before-snap)))
    (dolist (s sym-list)
      (condition-case err
          (let ((entry (assoc s before)))
            (if entry
                (progn (fset s (cdr entry)))
              (fmakunbound s)))
        (error (message "restore-functions: error restoring %s: %s" s err))))))

(defun restore-variables (var-list before-snap)
  "Restore variables in VAR-LIST to values recorded in BEFORE-SNAP.
If a variable was unbound before, `makunbound` will be called."
  (let ((before (alist-get :variables before-snap)))
    (dolist (v var-list)
      (condition-case err
          (let ((entry (assoc v before)))
            (if entry
                (set v (cdr entry))
              (makunbound v)))
        (error (message "restore-variables: error restoring %s: %s" v err))))))

(defun restore-global-keys (key-list before-snap)
  "Restore global keybindings for KEY-LIST using BEFORE-SNAP snapshot.
KEY-LIST are key-description strings (as produced by `key-description`)."
  (let ((before-keys (alist-get :global-keybindings before-snap)))
    (dolist (ks key-list)
      (condition-case err
          (let ((binding (cdr (assoc ks before-keys))))
            (if binding
                (global-set-key (kbd ks) binding)
              (global-unset-key (kbd ks))))
        (error (message "restore-global-keys: error for %s: %s" ks err))))))

(defun remove-hooks (hook-list before-snap)
  "For each hook in HOOK-LIST, remove functions that weren't present in BEFORE-SNAP."
  (let ((before-hooks (alist-get :hooks before-snap)))
    (dolist (h hook-list)
      (condition-case err
          (let* ((before (cdr (assoc h before-hooks)))
                 (now (and (boundp h) (symbol-value h))))
            (when (and (listp now))
              (dolist (fn now)
                (unless (member fn before)
                  (ignore-errors (remove-hook h fn))))))
        (error (message "remove-hooks: error for %s: %s" h err))))))

(defun cancel-new-timers (timers before-snap)
  "Cancel timers listed in TIMERS that were not present in BEFORE-SNAP."
  (let ((before (alist-get :timers before-snap)))
    (dolist (t timers)
      (condition-case err
          (unless (member t before)
            (when (timerp t) (cancel-timer t)))
        (error (message "cancel-new-timers: error for %s: %s" t err))))))

(defun kill-new-processes (proc-list before-snap)
  "Kill processes in PROC-LIST that were not present in BEFORE-SNAP."
  (let ((before (alist-get :processes before-snap)))
    (dolist (p proc-list)
      (condition-case err
          (unless (member p before)
            (when (and (processp p) (process-live-p p)) (delete-process p)))
        (error (message "kill-new-processes: error for %s: %s" p err))))))

(defun unload-new-features (feat-list before-snap)
  "Attempt to unload features in FEAT-LIST that were not present in BEFORE-SNAP.
This calls `unload-feature` with `force` (t). Best-effort."
  (let ((before (alist-get :features before-snap)))
    (dolist (f feat-list)
      (condition-case err
          (unless (member f before)
            (when (featurep f)
              (ignore-errors (unload-feature f t))))
        (error (message "unload-new-features: error for %s: %s" f err))))))

;;;; Convenience flow: compute diff and run restore steps

(defun snapshot-restore-from-diff (snap-before snap-after &optional do-restore)
  "Compute diff of SNAP-BEFORE and SNAP-AFTER and (optionally) attempt restores.
When DO-RESTORE is non-nil, run the common restore helpers on the diff.
Returns the diff plist."
  (let ((diff (emacs-snapshot-diff-safe snap-before snap-after)))
    (when do-restore
      ;; functions
      (when-let ((fadd (plist-get diff :functions-added)))
        (restore-functions fadd snap-before))
      (when-let ((fred (plist-get diff :functions-redefined)))
        (restore-functions fred snap-before))
      ;; variables
      (when-let ((vadd (plist-get diff :variables-added)))
        (restore-variables vadd snap-before))
      (when-let ((vch (plist-get diff :variables-changed)))
        (restore-variables vch snap-before))
      ;; keys
      (when-let ((kadd (plist-get diff :global-keys-added)))
        (restore-global-keys kadd snap-before))
      ;; hooks
      (when-let ((hadd (plist-get diff :hooks-added)))
        (remove-hooks hadd snap-before))
      (when-let ((hch (plist-get diff :hooks-changed)))
        (remove-hooks hch snap-before))
      ;; timers, processes, features
      (when-let ((tadd (plist-get diff :timers-added)))
        (cancel-new-timers tadd snap-before))
      (when-let ((padd (plist-get diff :processes-started)))
        (kill-new-processes padd snap-before))
      (when-let ((feat (plist-get diff :features-loaded)))
        (unload-new-features feat snap-before)))
    diff))

;;;; Small pretty-printer for diffs (safe)

(defun snapshot-diff-summary (diff)
  "Produce a compact summary string for DIFF (plist)."
  (mapconcat
   (lambda (item)
     (pcase item
       (`(:functions-added . ,v) (format "functions added: %d" (length v)))
       (`(:functions-redefined . ,v) (format "functions redefined: %d" (length v)))
       (`(:variables-added . ,v) (format "vars added: %d" (length v)))
       (`(:variables-changed . ,v) (format "vars changed: %d" (length v)))
       (`(:faces-added . ,v) (format "faces added: %d" (length v)))
       (`(:global-keys-added . ,v) (format "global keys added: %d" (length v)))
       (`(:hooks-added . ,v) (format "hooks added: %d" (length v)))
       (`(:features-loaded . ,v) (format "features loaded: %d" (length v)))
       (`(:timers-added . ,v) (format "timers added: %d" (length v)))
       (`(:processes-started . ,v) (format "processes started: %d" (length v)))
       (_ (format "%S" item))))
   " | "
   diff))

(provide 'emacs-snapshot-safe)
;;; emacs-snapshot-safe.el ends here
