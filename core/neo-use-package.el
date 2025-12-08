(require 'neo-list-utils)

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

(defun neo--alist-replace-key (key new-key new-value alist)
  "Replace KEY in ALIST with (NEW-KEY . NEW-VALUE).  
If KEY is not present, return ALIST unchanged."
  (if (assoc key alist)
    (neo--alist-append
     (neo--alist-remove-key key alist)
     new-key new-value)
    alist))

;; :hook
;; FOO -> (FOO . <package-name>-mode)
;; (FOO BAR) -> ((FOO BAR) . <package-name>-mode)
;; add a :depth and a :local, but with all the magic above is
;; difficult
;; TODO: check how :hook implements support for autoloads and do it as
;; we need to maintain equivalence even with :depth and :local
(defun neo--normalize-hooks (alist)
  "Normalize `:hook` arguments in a `use-package` declaration ALIST.
This is currently a stub and returns ALIST unchanged."
  alist)

(defun neo--normalize-use-package-arguments (args)
  "Normalize ARGS for `use-package` to a canonical form.
This involves removing `:doc` keywords and replacing `:builtin` with `:ensure nil`."
  (let* ((args-alist (neo--sectioned-list->alist args))
	 (args-alist (neo--alist-remove-key :doc args-alist))
	 ;; TODO we should check :ensure is not present or is compatible.
	 (args-alist (neo--alist-replace-key :builtin :ensure nil args-alist))
	 (args-alist (neo--normalize-hooks args-alist))
	 (args (neo--alist->sectioned-list args-alist)))
    args))

;; TODO this is very hacky. When working in Omega grand-parent for this file is emacs.
;; but when deployed, it will be the name of the user init directory, typically neo
(defun neo--publisher-name ()
  "Return the name of the directory two levels above PATH, or nil if none.
That corresponds to the publisher of an extension."
  (let* ((file (or load-file-name buffer-file-name))
	 (dir (directory-file-name (or (file-name-directory file) "")))
         (parent (file-name-directory dir)))
    (when parent
      (let ((grand-parent (file-name-nondirectory (directory-file-name parent))))
        (unless (string= grand-parent "") grand-parent)))))

(defun neo--author-name ()
  "Return the name of the directory containing FILE."
  (let ((file (or load-file-name buffer-file-name)))
    (if file
	(file-name-nondirectory
	 (directory-file-name
	  (file-name-directory file)))
       "unknown")))

(defun neo--alist-push (key value alist)
  "Push VALUE onto the list at KEY in ALIST, creating a new list if needed.
ALIST should be a variable holding an association list of the form ((key . list) ...)."
  (let ((cell (assoc key alist)))
    (if cell
        (setcdr cell (append (cdr cell) (list value)))  ; append new value to existing list
      (push (cons key (list value)) alist)))            ; create new key with singleton list
  alist)

(defmacro neo/use-package (name &rest args)
  "Augment `use-package` with Neo-specific tracking and filtering.

If the global variable neo/use-extensions is t, the use-package is
immediately executed, otherwise the raw `use-package` form is stored in
`neo--enabled-packages` indexed by (user . extension-base-name)."
  (declare (indent defun))
  (let* ((ensure (if (string= name "emacs") (list :ensure nil) '()))
;         (args (append (neo/filter-package-args args) ensure))
         (args (append (neo--normalize-use-package-arguments args) ensure))
         (file (or load-file-name buffer-file-name "unknown"))
         (user (neo--publisher-name))
         (extension (file-name-nondirectory
		     (directory-file-name
		      (file-name-directory file))))
         (key (cons user extension))
         (real-form `(use-package ,name ,@args)))
    (if neo/use-extensions
	`(setq neo--enabled-packages (neo--alist-push ',key ',real-form neo--enabled-packages))
      `(eval ',real-form))))

(provide 'neo-use-package)
