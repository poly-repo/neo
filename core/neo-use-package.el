;;; -*- lexical-binding: t -*-
(require 'cl-lib)
(require 'neo-list-utils)

(cl-defstruct (neo-merge-conflict (:constructor make-neo-merge-conflict))
  "A single dropped value from merging duplicate `neo/use-package' declarations.
PACKAGE-NAME is the package the conflict occurred in, SECTION is the
keyword section (e.g. `:custom', `:ensure'), SUB-KEY further identifies
the conflicting item within SECTION (e.g. the `:custom' variable
symbol), nil when SECTION has no sub-keys (e.g. `:ensure').
KEPT-SOURCE/KEPT-VALUE describe the value that survived the merge,
DROPPED-SOURCE/DROPPED-VALUE the value that lost. SOURCE values are
`(publisher . extension)' conses, the same shape used as
`neo--enabled-packages' keys."
  package-name section sub-key kept-source kept-value dropped-source dropped-value)

(cl-defstruct (neo-package-provenance (:constructor make-neo-package-provenance))
  "Provenance of a package's merged `neo/use-package' declaration.
NAME is the package name symbol. SOURCES is the ordered list of
`(publisher . extension)' conses that contributed a declaration.
MERGED-ARGS-ALIST is the final keyword-sectioned alist (as produced by
`neo--sectioned-list->alist') used to build the single `use-package'
form. SECTION-ORIGINS records, per section, which source first
contributed each surviving item. CONFLICTS is a list of
`neo-merge-conflict' structs describing every value that was dropped."
  name sources merged-args-alist section-origins conflicts)

(defconst neo--use-package-default-ensure '(:wait t)
  "Default `:ensure' value injected by `neo/use-package'.
Shared with the merge logic in `neo--merge-use-package-declarations' so
it can recognize a framework-injected default vs. a real user-supplied
recipe.")

(defconst neo--use-package-list-sections
  '(:config :init :hook :bind :bind* :custom-face :mode :interpreter
    :commands :defines :functions :requires :after :defer :demand)
  "Keyword sections merged by concatenating and de-duping `equal' forms.")

(defconst neo--use-package-mapping-sections '(:custom)
  "Keyword sections merged as a mapping keyed by each form's `car'.")

(defconst neo--use-package-scalar-sections '(:ensure)
  "Keyword sections holding a single value classified default-vs-real.")

(defconst neo--use-package-single-value-sections '(:ensure :if :disabled)
  "Output sections whose `section-origins' entry is a single SOURCE cons,
not a list of `(FORM . SOURCE)' pairs. Used by
`neo--format-package-provenance' to skip the per-item origin display for
these -- `:ensure' is `neo--use-package-scalar-sections'; `:if'/`:disabled'
are the two possible output keywords of
`neo--merge-use-package-condition-section', which likewise records a
single deciding source rather than one per surviving form.")

(defconst neo--use-package-condition-keywords '(:if :when :unless :disabled)
  "Keywords merged by `neo--merge-use-package-condition-section' into a
single `:if' (or bare `:disabled') in the replayed form, instead of being
concatenated like an ordinary list section. Native `use-package' treats
these four as one underlying concept -- `use-package-unalias-keywords'
rewrites `:when' to `:if' and `:unless X' to `:if (not X)', and
`use-package-merge-key-alist''s `:if' entry ANDs repeated `:if's within one
form -- so naive per-source concatenation produces a malformed
`(use-package NAME :if COND1 COND2 ...)' (`:if' takes exactly one argument).")

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

(defun neo/get-parent-directory-at-level (path n)
  "Return the name of the directory N levels above PATH."
  (let ((current-path (expand-file-name path)))
    ;; Climb N levels up
    (dotimes (_ n)
      (setq current-path (file-name-directory (directory-file-name current-path))))
    ;; At this point current-path is "/home/mav/.cache/neo/extensions/neo/"
    ;; We need to return just 'neo', so we strip the trailing slash and get the tail
    (file-name-nondirectory (directory-file-name current-path))))

;; TODO this is very hacky. When working in Omega grand-parent for this file is emacs.
;; but when deployed, it will be the name of the user init directory, typically neo
;; (defun neo--publisher-name ()
;;   "Return the name of the directory two levels above PATH, or nil if none.
;; That corresponds to the publisher of an extension."
;;   (let* ((file (or load-file-name buffer-file-name))
;; 	 (dir (directory-file-name (or (file-name-directory file) "")))
;;          (parent (file-name-directory dir)))
;;     (when parent
;;       (let ((grand-parent (file-name-nondirectory (directory-file-name parent))))
;;         (unless (string= grand-parent "") grand-parent)))))

(defun neo--publisher-name ()
  "Return the name of the directory two levels above PATH, or nil if none.
That corresponds to the publisher of an extension."
  (let* ((file (or load-file-name buffer-file-name))
	 (publisher (neo/get-parent-directory-at-level file 2)))
    (if (string= publisher "current")
	(neo/get-parent-directory-at-level file 3)
      publisher)))

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

(defun neo--prepare-use-package-form (form seen-packages)
  "Return FORM with duplicate installs disabled using SEEN-PACKAGES.

When FORM is a duplicate `use-package' declaration for a package already
present in SEEN-PACKAGES, force `:ensure nil' so the later declaration still
applies configuration without re-queueing the install."
  (if (and (listp form) (eq (car form) 'use-package))
      (let* ((name (cadr form))
             (args (cddr form))
             (duplicate (and seen-packages
                             (not (string= name "emacs"))
                             (gethash name seen-packages))))
        (when (and seen-packages
                   (not (string= name "emacs")))
          (puthash name t seen-packages))
        (if duplicate
            (let* ((args-alist (neo--sectioned-list->alist args))
                   (args-alist (if (assoc :ensure args-alist)
                                   (neo--alist-replace-key :ensure :ensure nil args-alist)
                                 (neo--alist-append args-alist :ensure nil)))
                   (args (neo--alist->sectioned-list args-alist)))
              `(use-package ,name ,@args))
          form))
    form))

(defun neo--format-source-key (source-key)
  "Return a human-readable \"publisher:extension\" string for SOURCE-KEY.
SOURCE-KEY is a `(publisher . extension)' cons, the same shape used as
`neo--enabled-packages' keys."
  (if source-key
      (format "%s:%s" (car source-key) (cdr source-key))
    "unknown"))

(defun neo--merge-use-package-list-section (section pairs)
  "Merge list SECTION across PAIRS, forms concatenated and `equal'-deduped.
PAIRS is a list of `(SOURCE . ARGS-ALIST)', ARGS-ALIST shaped as
returned by `neo--sectioned-list->alist'. SECTION must be one of
`neo--use-package-list-sections'.

Returns `(VALUE . ORIGINS)'. VALUE is the deduped list of forms in
first-occurrence order, the symbol `neo/no-args' when every source that
declared SECTION used the bare-flag form, or `neo/absent' when no
source declared SECTION at all. ORIGINS is an alist of `(FORM . SOURCE)'
for each surviving form, or, when VALUE is `neo/no-args', a single
`(SECTION . SOURCE)' pair naming the first source that declared the
flag."
  (let (merged origins flag-source present)
    (dolist (pair pairs)
      (let* ((source (car pair))
             (entry (assoc section (cdr pair))))
        (when entry
          (setq present t)
          (if (eq (cdr entry) 'neo/no-args)
              (unless flag-source (setq flag-source source))
            (dolist (form (cdr entry))
              (unless (cl-member form merged :test #'equal)
                (push form merged)
                (push (cons form source) origins)))))))
    (cond
     (merged (cons (nreverse merged) (nreverse origins)))
     (flag-source (cons 'neo/no-args (list (cons section flag-source))))
     (present (cons 'neo/no-args nil))
     (t (cons 'neo/absent nil)))))

(defun neo--merge-use-package-mapping-section (name section pairs)
  "Merge mapping SECTION across PAIRS for package NAME.
PAIRS is a list of `(SOURCE . ARGS-ALIST)'. SECTION must be one of
`neo--use-package-mapping-sections'; each of its forms is keyed by its
own `car' (e.g. the variable symbol in a `:custom' entry). An unseen key
is added; a key seen again with an `equal' form is silently deduped; a
key seen again with a different form keeps the first and records a
`neo-merge-conflict'.

Returns `(VALUE ORIGINS CONFLICTS)': VALUE is the merged list of forms,
ORIGINS is an alist of `(KEY . SOURCE)', CONFLICTS a list of
`neo-merge-conflict' structs."
  (let (merged origins conflicts (seen (make-hash-table :test 'eq)))
    (dolist (pair pairs)
      (let* ((source (car pair))
             (entry (assoc section (cdr pair)))
             (forms (and entry (listp (cdr entry)) (cdr entry))))
        (dolist (form forms)
          (let* ((key (car form))
                 (existing (gethash key seen)))
            (cond
             ((null existing)
              (puthash key (cons source form) seen)
              (push form merged)
              (push (cons key source) origins))
             ((not (equal (cdr existing) form))
              (push (make-neo-merge-conflict
                     :package-name name :section section :sub-key key
                     :kept-source (car existing) :kept-value (cdr existing)
                     :dropped-source source :dropped-value form)
                    conflicts)))))))
    (list (nreverse merged) (nreverse origins) (nreverse conflicts))))

(defun neo--merge-use-package-ensure-section (name section pairs)
  "Merge scalar `:ensure'-shaped SECTION across PAIRS for package NAME.
PAIRS is a list of `(SOURCE . ARGS-ALIST)'. Each source's value is
classified as \"default\" (`equal' to `neo--use-package-default-ensure')
or \"real\". When NAME is \"emacs\", SECTION always merges to nil with
no conflict check. Otherwise: all-default sources merge to the default;
exactly one real value wins outright; two or more `equal' real values
merge to that value; two or more differing real values keep the first
and record a `neo-merge-conflict' for each of the rest.

Returns `(VALUE ORIGIN CONFLICTS)': ORIGIN is the source whose real
value won, or nil when VALUE is the framework default."
  (if (string= name "emacs")
      (list nil nil nil)
    (let (real-pairs conflicts)
      (dolist (pair pairs)
        (let ((entry (assoc section (cdr pair))))
          (when (and entry (listp (cdr entry)))
            (let ((value (cadr entry)))
              (unless (equal value neo--use-package-default-ensure)
                (push (cons (car pair) value) real-pairs))))))
      (setq real-pairs (nreverse real-pairs))
      (if (null real-pairs)
          (list neo--use-package-default-ensure nil nil)
        (let* ((first (car real-pairs))
               (first-source (car first))
               (first-value (cdr first)))
          (dolist (pair (cdr real-pairs))
            (unless (equal (cdr pair) first-value)
              (push (make-neo-merge-conflict
                     :package-name name :section section :sub-key nil
                     :kept-source first-source :kept-value first-value
                     :dropped-source (car pair) :dropped-value (cdr pair))
                    conflicts)))
          (list first-value first-source (nreverse conflicts)))))))

(defun neo--use-package-condition-forms (keyword args-alist)
  "Return the forms KEYWORD maps to in ARGS-ALIST, or nil.
ARGS-ALIST is shaped as returned by `neo--sectioned-list->alist'."
  (let ((entry (assoc keyword args-alist)))
    (and entry (listp (cdr entry)) (cdr entry))))

(defun neo--use-package-source-condition (args-alist)
  "Reduce ARGS-ALIST's :if/:when/:unless/:disabled to one condition.
Returns `neo/disabled' when `:disabled' is present -- matching native
`use-package', whose `:disabled' presence makes every other keyword
irrelevant, so other condition keywords are ignored once `:disabled' is
seen. Returns `neo/absent' when none of `neo--use-package-condition-keywords'
appears. Otherwise returns a single Lisp form: this source's own `:if'
forms, then `:when' forms, then negated `:unless' forms, AND-combined in
that order (a single term is returned unwrapped; two or more are wrapped
in `(and ...)')."
  (if (assoc :disabled args-alist)
      'neo/disabled
    (let (terms)
      (dolist (form (neo--use-package-condition-forms :if args-alist))
        (push form terms))
      (dolist (form (neo--use-package-condition-forms :when args-alist))
        (push form terms))
      (dolist (form (neo--use-package-condition-forms :unless args-alist))
        (push (list 'not form) terms))
      (setq terms (nreverse terms))
      (cond
       ((null terms) 'neo/absent)
       ((null (cdr terms)) (car terms))
       (t (cons 'and terms))))))

(defun neo--use-package-condition-trivially-false-p (condition)
  "Non-nil when CONDITION is structurally guaranteed false without
evaluating it: `neo/disabled', or a literal `nil' (e.g. from `:if nil')."
  (or (eq condition 'neo/disabled) (null condition)))

(defun neo--use-package-condition-conflict (name kept-source kept-value dropped-source dropped-value)
  "Build a `neo-merge-conflict' for NAME's merged :if/:disabled section.
Shares shape across both condition-merge branches: SECTION is `:if',
SUB-KEY nil."
  (make-neo-merge-conflict
   :package-name name :section :if :sub-key nil
   :kept-source kept-source :kept-value kept-value
   :dropped-source dropped-source :dropped-value dropped-value))

(defun neo--merge-use-package-condition-disable (name conditions false-pairs)
  "Merge CONDITIONS for NAME when at least one source is trivially-false.
CONDITIONS is `((SOURCE . CONDITION) ...)' as produced by
`neo--use-package-source-condition'; FALSE-PAIRS is the subset that is
trivially-false per `neo--use-package-condition-trivially-false-p'. A
`:disabled' source wins over a merely `:if nil' source; ties within each
class keep declaration order. Unanimous agreement (every source
trivially-false) produces no conflict; otherwise every other source is
recorded as silently overridden.

Returns `(VALUE ORIGIN CONFLICTS)', see `neo--merge-use-package-condition-section'."
  (let* ((disabling (or (cl-find-if (lambda (c) (eq (cdr c) 'neo/disabled)) false-pairs)
                         (car false-pairs)))
         (value (if (eq (cdr disabling) 'neo/disabled) 'neo/disabled nil))
         (origin (car disabling))
         (kept-value (if (eq value 'neo/disabled) '(:disabled) (list :if value)))
         (conflicts nil))
    (unless (= (length false-pairs) (length conditions))
      (dolist (pair conditions)
        (unless (memq pair false-pairs)
          (push (neo--use-package-condition-conflict
                 name origin kept-value (car pair)
                 (if (eq (cdr pair) 'neo/absent) t (list :if (cdr pair))))
                conflicts))))
    (list value origin (nreverse conflicts))))

(defun neo--merge-use-package-condition-and (name conditions)
  "Merge CONDITIONS for NAME when no source is trivially-false.
CONDITIONS is shaped as in `neo--merge-use-package-condition-disable'.
Distinct real conditions AND-combine silently -- mirrors `use-package''s
own repeated-`:if' semantics, since every contributing source already
opted into conditional loading. A source that declared no condition at
all (implicitly unconditional) is flagged as a conflict, since it
silently gains the AND-combined restriction it never asked for.

Returns `(VALUE ORIGIN CONFLICTS)', see `neo--merge-use-package-condition-section'."
  (let* ((real-pairs (cl-remove-if (lambda (c) (eq (cdr c) 'neo/absent)) conditions))
         (unconditional-pairs (cl-remove-if-not (lambda (c) (eq (cdr c) 'neo/absent)) conditions))
         (distinct nil))
    (dolist (pair real-pairs)
      (unless (cl-member (cdr pair) distinct :test #'equal)
        (push (cdr pair) distinct)))
    (setq distinct (nreverse distinct))
    (let* ((value (if (cdr distinct) (cons 'and distinct) (car distinct)))
           (origin (car (car real-pairs)))
           (conflicts nil))
      (dolist (pair unconditional-pairs)
        (push (neo--use-package-condition-conflict
               name origin (list :if value) (car pair) t)
              conflicts))
      (list value origin (nreverse conflicts)))))

(defun neo--merge-use-package-condition-section (name pairs)
  "Merge :if/:when/:unless/:disabled across PAIRS for package NAME.
PAIRS is a list of `(SOURCE . ARGS-ALIST)'. Returns `(VALUE ORIGIN
CONFLICTS)', shaped like `neo--merge-use-package-ensure-section': VALUE is
`neo/disabled' (emit a bare `:disabled'), `neo/absent' (emit nothing), or
a single Lisp form (emit `:if VALUE'). ORIGIN is the source that decided
VALUE, or nil when VALUE is `neo/absent'.

A source that declares none of `neo--use-package-condition-keywords' is
treated as unconditional (equivalent to `t'), matching `use-package''s own
default. Conflicts are recorded only when the merge silently overrides a
source's own expectation -- see `neo--merge-use-package-condition-disable'
and `neo--merge-use-package-condition-and'. Two genuinely conditional
sources AND-combining is not itself a conflict."
  (let (conditions)
    (dolist (pair pairs)
      (push (cons (car pair) (neo--use-package-source-condition (cdr pair))) conditions))
    (setq conditions (nreverse conditions))
    (if (cl-every (lambda (c) (eq (cdr c) 'neo/absent)) conditions)
        (list 'neo/absent nil nil)
      (let ((false-pairs (cl-remove-if-not
                           (lambda (c) (neo--use-package-condition-trivially-false-p (cdr c)))
                           conditions)))
        (if false-pairs
            (neo--merge-use-package-condition-disable name conditions false-pairs)
          (neo--merge-use-package-condition-and name conditions))))))

(defun neo--merge-use-package-declarations (name ordered-source-args-pairs)
  "Merge ORDERED-SOURCE-ARGS-PAIRS into one args-alist for NAME.
ORDERED-SOURCE-ARGS-PAIRS is a list of `(SOURCE-KEY . ARGS-ALIST)' in
dependency-first order, each ARGS-ALIST shaped as returned by
`neo--sectioned-list->alist'. Returns a `neo-package-provenance'.

Pure -- never calls `display-warning'; recording conflicts is this
function's job, warning about them is the caller's."
  (let ((sources (mapcar #'car ordered-source-args-pairs))
        (merged-args-alist nil)
        (section-origins nil)
        (conflicts nil))
    (dolist (section neo--use-package-list-sections)
      (let* ((result (neo--merge-use-package-list-section section ordered-source-args-pairs))
             (value (car result))
             (origins (cdr result)))
        (unless (eq value 'neo/absent)
          (push (cons section value) merged-args-alist)
          (when origins
            (push (cons section origins) section-origins)))))
    (dolist (section neo--use-package-mapping-sections)
      (let* ((result (neo--merge-use-package-mapping-section name section ordered-source-args-pairs))
             (value (nth 0 result))
             (origins (nth 1 result))
             (section-conflicts (nth 2 result)))
        (when value
          (push (cons section value) merged-args-alist))
        (when origins
          (push (cons section origins) section-origins))
        (setq conflicts (append conflicts section-conflicts))))
    (dolist (section neo--use-package-scalar-sections)
      (let* ((result (neo--merge-use-package-ensure-section name section ordered-source-args-pairs))
             (value (nth 0 result))
             (origin (nth 1 result))
             (section-conflicts (nth 2 result)))
        (push (cons section (list value)) merged-args-alist)
        (when origin
          (push (cons section origin) section-origins))
        (setq conflicts (append conflicts section-conflicts))))
    ;; :if/:when/:unless/:disabled are handled as one call, not a dolist
    ;; over `neo--use-package-condition-keywords', because unlike the three
    ;; loops above (each of which merges SECTION -> SECTION), this one
    ;; collapses 4 distinct input keywords into a single :if (or bare
    ;; :disabled) output keyword -- see
    ;; `neo--merge-use-package-condition-section'.
    (let* ((result (neo--merge-use-package-condition-section name ordered-source-args-pairs))
           (value (nth 0 result))
           (origin (nth 1 result))
           (section-conflicts (nth 2 result)))
      (cond
       ((eq value 'neo/disabled)
        (push (cons :disabled 'neo/no-args) merged-args-alist))
       ((not (eq value 'neo/absent))
        (push (cons :if (list value)) merged-args-alist)))
      (when origin
        (push (cons (if (eq value 'neo/disabled) :disabled :if) origin) section-origins))
      (setq conflicts (append conflicts section-conflicts)))
    (make-neo-package-provenance
     :name name
     :sources sources
     :merged-args-alist (nreverse merged-args-alist)
     :section-origins (nreverse section-origins)
     :conflicts conflicts)))

(defun neo--format-package-provenance (provenance)
  "Return a human-readable description of PROVENANCE.
PROVENANCE is a `neo-package-provenance', typically looked up from
`neo--package-provenance-table'. Used as the buffer contents for
`neo/describe-package-source'."
  (let* ((name (neo-package-provenance-name provenance))
         (sources (neo-package-provenance-sources provenance))
         (conflicts (neo-package-provenance-conflicts provenance))
         (section-origins (neo-package-provenance-section-origins provenance))
         (ensure-value (cadr (assoc :ensure (neo-package-provenance-merged-args-alist provenance)))))
    (with-temp-buffer
      (insert (format "Package: %s\n\n" name))
      (if (<= (length sources) 1)
          (insert (format "Single declaration, from %s. No merge occurred.\n"
                          (neo--format-source-key (car sources))))
        (progn
          (insert (format "Merged from %d declarations:\n" (length sources)))
          (dolist (source sources)
            (insert (format "  - %s\n" (neo--format-source-key source))))
          (insert "\nContributions by section:\n")
          (dolist (section-entry section-origins)
            (let ((section (car section-entry)))
              (unless (memq section neo--use-package-single-value-sections)
                (insert (format "  %s:\n" section))
                (dolist (origin (cdr section-entry))
                  (insert (format "    %S <- %s\n"
                                  (car origin) (neo--format-source-key (cdr origin))))))))))
      (insert (format "\nResolved :ensure: %S\n" ensure-value))
      (if conflicts
          (progn
            (insert (format "\n%d conflict(s):\n" (length conflicts)))
            (dolist (conflict conflicts)
              (insert (format "  - [%s%s] kept %S from %s, dropped %S from %s\n"
                              (neo-merge-conflict-section conflict)
                              (if (neo-merge-conflict-sub-key conflict)
                                  (format " %S" (neo-merge-conflict-sub-key conflict))
                                "")
                              (neo-merge-conflict-kept-value conflict)
                              (neo--format-source-key (neo-merge-conflict-kept-source conflict))
                              (neo-merge-conflict-dropped-value conflict)
                              (neo--format-source-key (neo-merge-conflict-dropped-source conflict))))))
        (insert "\nNo conflicts.\n"))
      (buffer-string))))

(defun neo--collect-package-sources (sorted-slugs installed-map enabled-packages)
  "Group ENABLED-PACKAGES's queued forms by package name.

Walks SORTED-SLUGS (a list of \"publisher:name\" strings in topological,
dependency-first order) and, for each installed extension found in
INSTALLED-MAP, looks up its queued `use-package' forms in
ENABLED-PACKAGES (an alist keyed by `(publisher . extension)' conses, the
same shape produced by `neo/use-package'). INSTALLED-MAP entries are
`neo/installation' structs (see `neo-extensions.el').

Returns `((NAME . ((SOURCE-KEY . ARGS-ALIST) ...)) ...)' in
first-appearance order; each inner list is in dependency-first order,
ARGS-ALIST shaped as returned by `neo--sectioned-list->alist'.

Pure -- no eval, no side effects."
  (let (order (by-name (make-hash-table :test 'equal)))
    (dolist (slug-string sorted-slugs)
      (when-let* ((installation (gethash slug-string installed-map))
                  (slug (neo/installation-extension-slug installation))
                  (key (cons (neo/extension-slug-publisher slug)
                             (neo/extension-slug-name slug)))
                  (forms (cdr (assoc key enabled-packages))))
        (dolist (form forms)
          (when (and (listp form) (eq (car form) 'use-package))
            (let* ((name (cadr form))
                   (args-alist (neo--sectioned-list->alist (cddr form)))
                   (entry (gethash name by-name)))
              (if entry
                  (setcdr entry (append (cdr entry) (list (cons key args-alist))))
                (puthash name (list (cons key args-alist)) by-name)
                (push name order)))))))
    (mapcar (lambda (name) (cons name (gethash name by-name)))
            (nreverse order))))

(defmacro neo/use-package (name &rest args)
  "Augment `use-package` with Neo-specific tracking and filtering.

If the global variable neo/use-extensions is t, the use-package is
immediately executed, otherwise the raw `use-package` form is stored in
`neo--enabled-packages` indexed by (user . extension-base-name)."
  (declare (indent defun))
  (let* ((args (neo--normalize-use-package-arguments args))
         (args (if (memq :ensure args)
                   args
                 (append args (if (string= name "emacs")
                                  '(:ensure nil)
                                (list :ensure neo--use-package-default-ensure)))))
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
