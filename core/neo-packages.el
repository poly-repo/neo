;; -*- lexical-binding: t -*-

(require 'map)


;;; NOTE this first definition wins so that we don't have to remember
;;; to modify the installer and we can simply copy and paste it.
;;; It would be great if elpaca provided that as a downloadable asset
;;; with each release and a summary of changes to go with it, but we
;;; live with what we have and elpaca is great anyhow.
(makunbound 'elpaca-directory)
(defvar elpaca-directory (expand-file-name "elpaca/" no-littering-var-directory))

;;; The following is the installer copied from the elpaca github
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))


;;; TODO: elisp autocompletion on :custom should be for variables, not functions)

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

(defun neo--sectioned-list->alist (data)
  "Convert a flat sectioned keyword list into an alist.

DATA is a list where keywords (e.g., :vars, :custom) are followed by grouped
forms (e.g., (foo t)). Returns an alist of (KEY . LIST-OF-FORMS).

Duplicate keys are merged."  
  (let ((result '())
        (current-key nil))
    (dolist (item data)
      (if (keywordp item)
          (setq current-key item)
        (when current-key
          (let ((existing (alist-get current-key result nil nil #'eq)))
            (setf (alist-get current-key result nil nil #'eq)
                  (append existing (list item)))))))
    result))

(defun neo--alist->sectioned-list (alist)
  "Convert an ALIST of grouped keyword sections back into a flat sectioned list.

Each key maps to a list of forms. Produces a flat list like:
  (:key form1 form2 ... :next-key ...)"
  (apply #'append
         (mapcar (lambda (entry)
                   (let ((key (car entry))
                         (forms (cdr entry)))
                     (cons key forms)))
                 alist)))

(defun neo--alist-append (alist key value)
  "Add VALUE to the list associated with KEY in ALIST.

If KEY is not present, insert a new entry with (KEY . (VALUE)).
Returns the updated ALIST."
  (let ((entry (assoc key alist)))
    (if entry
        (setcdr entry (append (cdr entry) (list value)))
      (setq alist (cons (cons key (list value)) alist))))
  alist)

(defun neo--alist-remove-key (key alist)
  "Return a new ALIST with all entries for KEY removed.

Uses `eq` for key comparison, like `assq-delete-all`."
  (assq-delete-all key alist))


(defun neo--normalize-use-package-arguments (args)
  (let* ((args-alist (neo--sectioned-list->alist args))
	 (args-alist (neo--alist-remove-key :doc args-alist))
	 (args (neo--alist->sectioned-list args-alist)))
    args))

(defmacro neo/use-package (name &rest args)
  "Augment `use-package` with Neo-specific tracking and filtering.

Stores the raw `use-package` form in `neo--enabled-packages`
indexed by (user . extension-base-name)."
  (declare (indent defun))
  (let* ((ensure (if (string= name "emacs") (list :ensure nil) '()))
;         (args (append (neo/filter-package-args args) ensure))
         (args (append (neo--normalize-use-package-arguments args) ensure))
         (file (or load-file-name buffer-file-name "unknown"))
         (user (user-login-name))
         (extension (file-name-base file))
         (key (cons user extension))
         (real-form `(use-package ,name ,@args))
         (existing (alist-get key neo--enabled-packages nil nil #'equal)))
    ;; Store the raw form, not the expanded one
    `(setq neo--enabled-packages
           (cons (cons ',key (cons ',real-form ',existing))
                 (assq-delete-all ',key neo--enabled-packages)))))

(defun neo/replay-extension-packages (&optional user extension)
  "Replay all stored `use-package` expansions.

If USER and EXTENSION are provided, only replays that entry."
  (interactive)
  (dolist (entry neo--enabled-packages)
    (let ((key (car entry))
          (forms (cdr entry)))
      (when (or (not user)
                (and (equal user (car key))
                     (equal extension (cdr key))))
        (dolist (form forms)
          (eval form))))))


(elpaca
 elpaca-use-package
 ;; Enable :elpaca use-package keyword.
 (elpaca-use-package-mode)
 ;; Assume :elpaca t unless otherwise specified.
 (setq elpaca-use-package-by-default t))

(defun neo--elpaca-bury-logs-if-clean ()
  "Bury *elpaca-logs* buffer unless it contains 'error' or warnings."
  (let ((buf (get-buffer "*elpaca-logs*")))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        (goto-char (point-min))
        (if (re-search-forward "\\(error\\|failed\\|warning\\)" nil t)
            (message "elpaca log contains errors or warnings")
          (bury-buffer buf))))))

(add-hook 'elpaca-after-init-hook #'neo--elpaca-bury-logs-if-clean)


;;; TODO: figure out where duplicate packages get queued
;;; till then is very annoying to see things like:
;;; ⛔ Warning (emacs): Duplicate item queued: ace-window
;;; ⛔ Warning (emacs): Duplicate item queued: magit
;;; ⛔ Warning (emacs): Duplicate item queued: org-roam
;(setq warning-minimum-level :error)

;; Block until current queue processed.
(elpaca-wait)

(provide 'neo-packages)
