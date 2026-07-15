;; -*- lexical-binding: t -*-
;(require 'neo-extensions)
;(require 'neo-extensions-digest)
(require 'neo-elpaca)
(require 'neo-use-package)

;; TODO: resolve the mess w/ :ensure-system-packages
;; TODO: elisp autocompletion on :custom should be for variables, not functions)

;;; ensure-system-package uses an async buffer (in a side window) which in turns
;;; causes the dashboard to be locked in there, a small window at the bottom of the
;;; frame that cannot even been grown.
;;; TODO: fix the mess for real
;;; for now, I just disable this, as I have everything installed.
(defcustom neo/ignore-ensure-system-package t
  "If non-nil, disables the use of `ensure-system-package` in `neo/use-package`.

This is useful when system dependencies are already satisfied or managed externally.
But it was introduced because elpaca had a problem with it:
  ensure-system-package uses an async buffer (in a side window) which in turns
  causes the dashboard to be locked in there, a small window at the bottom of the
  frame that cannot even been grown.
"
  :type 'boolean
  :group 'neo-packages)

;(defvar neo--installed-packages nil) ; TODO is this used

;;; NOTE: if performance becomes problematic we can move to a hash
;;; table:
;;; (defvar neo/extension-package-map (make-hash-table :test #'equal)
;;;  "Mapping of (USER . EXTENSION) to list of packages used.")
(defvar neo--enabled-packages nil
  "Alist mapping (USER . EXTENSION) to a list of unexpanded `neo/use-package` forms.")

(defvar neo--replayed-package-installs nil
  "Hash table tracking packages already replayed during startup.")

(defvar neo--package-provenance-table nil
  "Hash table (package name symbol -> `neo-package-provenance').
Populated by `neo/replay-installed-extensions-packages' on every full
bootstrap. Deliberately NOT let-bound to that call -- it must outlive
the bootstrap pass so `neo/describe-package-source' can query it at any
later time.")

(setq package-install-upgrade-built-in t)

(setopt package-archives
        '(("gnu" . "https://elpa.gnu.org/packages/")
          ("nongnu" . "https://elpa.nongnu.org/nongnu/")
          ("melpa" . "https://melpa.org/packages/")))


(defun neo/replay-extension-packages (&optional slug)
  "Replay all stored `use-package` expansions.

If SLUG is provided, a `neo/extension-slug` object, only replays that entry.

This is the single-extension interactive dev-reload path and has no
cross-extension visibility, so it cannot merge duplicate declarations
the way `neo/replay-installed-extensions-packages' does. Replaying a
single extension after a full merged bootstrap can therefore re-eval a
package whose declaration was already merged with another extension's --
a pre-existing risk class, not a regression introduced by merging."
  (interactive)
  (let ((user (when slug (neo/extension-slug-publisher slug)))
        (extension (when slug (neo/extension-slug-name slug))))
    (dolist (entry neo--enabled-packages)
      (let ((key (car entry))
            (forms (cdr entry)))
        (when (or (not slug)
                  (and (equal user (car key))
                       (equal extension (cdr key))))
          (dolist (form forms)
            (eval (neo--prepare-use-package-form
                   form
                   neo--replayed-package-installs))))))))

;; NOTE: `neo--collect-package-sources' lives in `neo-use-package.el', not
;; here, even though it groups entries from `neo--enabled-packages' (defined
;; above). It is pure (no eval, no Elpaca, no `no-littering'/elpaca bootstrap
;; side effects), so keeping it alongside the other pure merge-logic in
;; `neo-use-package.el' lets it -- and the tests that exercise it -- load
;; without pulling in this file's top-level Elpaca bootstrap.

(defun neo--warn-use-package-conflict (conflict)
  "Emit a `display-warning' describing CONFLICT, a `neo-merge-conflict'."
  (display-warning
   'neo-use-package
   (format "neo/use-package: merging %s -- %s%s conflict: kept %S from %s, dropped %S from %s"
           (neo-merge-conflict-package-name conflict)
           (neo-merge-conflict-section conflict)
           (if (neo-merge-conflict-sub-key conflict)
               (format " %S" (neo-merge-conflict-sub-key conflict))
             "")
           (neo-merge-conflict-kept-value conflict)
           (neo--format-source-key (neo-merge-conflict-kept-source conflict))
           (neo-merge-conflict-dropped-value conflict)
           (neo--format-source-key (neo-merge-conflict-dropped-source conflict)))
   :warning))

(defun neo--replay-merged-package (name source-pairs)
  "Merge SOURCE-PAIRS, warn on conflicts, and eval the resulting form for NAME.

SOURCE-PAIRS is `((SOURCE-KEY . ARGS-ALIST) ...)' as produced by
`neo--collect-package-sources'. Records the resulting
`neo-package-provenance' in `neo--package-provenance-table' before
evaluating the merged `use-package' form, so a failure evaluating the
form still leaves provenance inspectable via
`neo/describe-package-source'."
  (let* ((provenance (neo--merge-use-package-declarations name source-pairs))
         (args (neo--alist->sectioned-list
                (neo-package-provenance-merged-args-alist provenance))))
    (puthash name provenance neo--package-provenance-table)
    (dolist (conflict (neo-package-provenance-conflicts provenance))
      (neo--warn-use-package-conflict conflict))
    (eval `(use-package ,name ,@args) t)))

(defun neo/describe-package-source (package-name)
  "Show which extension(s) contributed which parts of PACKAGE-NAME's
merged `use-package' declaration, in buffer `*neo package source:
PACKAGE-NAME*'."
  (interactive
   (list (intern (completing-read
                  "Package: "
                  (when neo--package-provenance-table
                    (let (names)
                      (maphash (lambda (k _v) (push (symbol-name k) names))
                               neo--package-provenance-table)
                      names))
                  nil t))))
  (let ((provenance (and neo--package-provenance-table
                         (gethash package-name neo--package-provenance-table))))
    (with-current-buffer (get-buffer-create (format "*neo package source: %s*" package-name))
      (erase-buffer)
      (if provenance
          (insert (neo--format-package-provenance provenance))
        (insert (format "No provenance recorded for %s.\n\nEither the framework has not finished bootstrapping yet, or %s was never declared via `neo/use-package'.\n"
                        package-name package-name)))
      (pop-to-buffer (current-buffer)))))

(defun neo--packages-with-conflicts ()
  "Return an alist (NAME . CONFLICTS) for every package with recorded
merge conflicts, sorted alphabetically by NAME. Empty when
`neo--package-provenance-table' is unset or nothing conflicted."
  (let (result)
    (when neo--package-provenance-table
      (maphash (lambda (name provenance)
                 (when-let* ((conflicts (neo-package-provenance-conflicts provenance)))
                   (push (cons name conflicts) result)))
               neo--package-provenance-table))
    (sort result (lambda (a b) (string< (symbol-name (car a)) (symbol-name (car b)))))))

(defun neo/package-conflict-count ()
  "Return the total number of recorded `neo/use-package' merge conflicts,
summed across every package in `neo--package-provenance-table'."
  (apply #'+ (mapcar (lambda (entry) (length (cdr entry)))
                     (neo--packages-with-conflicts))))

(defvar neo-package-conflicts-summary-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "g") #'neo/package-conflicts-summary)
    (define-key map (kbd "TAB") #'forward-button)
    (define-key map (kbd "<backtab>") #'backward-button)
    map)
  "Keymap for `neo-package-conflicts-summary-mode'.")

(define-derived-mode neo-package-conflicts-summary-mode special-mode "Neo-Package-Conflicts"
  "Major mode listing every `neo/use-package' merge conflict by package.

\\{neo-package-conflicts-summary-mode-map}")

(defun neo--insert-package-conflict-line (conflict)
  "Insert one summary line describing CONFLICT, a `neo-merge-conflict'."
  (insert (format "    - %s%s: kept %S from %s, dropped %S from %s\n"
                  (neo-merge-conflict-section conflict)
                  (if (neo-merge-conflict-sub-key conflict)
                      (format " %S" (neo-merge-conflict-sub-key conflict))
                    "")
                  (neo-merge-conflict-kept-value conflict)
                  (neo--format-source-key (neo-merge-conflict-kept-source conflict))
                  (neo-merge-conflict-dropped-value conflict)
                  (neo--format-source-key (neo-merge-conflict-dropped-source conflict)))))

(defun neo--insert-package-conflict-entry (name conflicts)
  "Insert a button for package NAME followed by each of its CONFLICTS.
Activating the button opens `neo/describe-package-source' for NAME."
  (insert-text-button (symbol-name name)
                       'action (lambda (_button) (neo/describe-package-source name))
                       'follow-link t
                       'help-echo "RET: describe this package's merge provenance")
  (insert (format "  (%d conflict%s)\n" (length conflicts) (if (= (length conflicts) 1) "" "s")))
  (mapc #'neo--insert-package-conflict-line conflicts)
  (insert "\n"))

(defun neo/package-conflicts-summary ()
  "Show every package whose merged `neo/use-package' declaration had a
conflict, in buffer `*neo package conflicts*'. Each package name is a
button opening `neo/describe-package-source' for it."
  (interactive)
  (let ((packages (neo--packages-with-conflicts)))
    (with-current-buffer (get-buffer-create "*neo package conflicts*")
      (neo-package-conflicts-summary-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if (null packages)
            (insert "No `neo/use-package' merge conflicts recorded.\n")
          (insert (format "%d package(s) with merge conflicts:\n\n" (length packages)))
          (dolist (entry packages)
            (neo--insert-package-conflict-entry (car entry) (cdr entry)))))
      (pop-to-buffer (current-buffer)))))

;;; NOTE: Elpaca emits "Duplicate item ID queued: X" via plain `warn' (warning
;;; type `emacs'), so it cannot be suppressed by type without also hiding
;;; unrelated `emacs' warnings — we deliberately do NOT blanket-suppress it.
;;; The root cause is handled by `neo--elpaca-enqueue-deduplicate' (neo-elpaca.el),
;;; which returns the already-queued entry instead of re-queueing.  Precisely
;;; typed benign warnings (defvaralias, lexical-binding) are suppressed in
;;; early-init.el via `warning-suppress-types'.

;; probably need to ensure it is run early
(use-package no-littering
  :ensure t
  :init
  ;; We define these in early-init.el so everything can be kept out of the way
  ;; in particular elpaca and eln-cache
  ;(setq no-littering-etc-directory (expand-file-name ".litter/config" user-emacs-directory))
  ;(setq no-littering-var-directory (expand-file-name ".litter/data" user-emacs-directory))
  :config
  (no-littering-theme-backups)
  (setq auto-save-file-name-transforms
        `((".*"
           ,(no-littering-expand-var-file-name "auto-save/")
           t))))


(provide 'neo-packages)
