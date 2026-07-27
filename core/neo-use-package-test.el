;;; neo-use-package-test.el --- Tests for neo/use-package -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'ert)

(unless (featurep 'dash)
  (defun -partition-before-pred (pred list)
    "Partition LIST before items matching PRED."
    (let ((result nil)
          (current nil))
      (dolist (item list)
        (when (and current (funcall pred item))
          (push (nreverse current) result)
          (setq current nil))
        (push item current))
      (when current
        (push (nreverse current) result))
      (nreverse result)))
(provide 'dash))

;; `neo--collect-package-sources' (in neo-use-package.el) reads
;; `neo/installation'/`neo/extension-slug' structs, defined in
;; neo-extensions.el; give it the same cache/config preamble
;; neo-extensions-test.el uses so `require' succeeds standalone.
(require 'neo-early-init-utils)
(defvar neo/cache-directory temporary-file-directory)
(defvar neo/config-directory temporary-file-directory)
(require 'neo-extensions)

(require 'neo-use-package)

(defconst neo-use-package-test--file
  "/tmp/neo/extensions/extensions/neo/test/neo-test.el")

(defvar neo/use-extensions t)

(ert-deftest neo/builtin-feature-p-recognizes-emacs ()
  "Recognize the `use-package' Emacs pseudo-feature by symbol or string."
  (should (neo/builtin-feature-p 'emacs))
  (should (neo/builtin-feature-p "emacs")))

(ert-deftest neo/builtin-feature-p-prefers-package-metadata ()
  "Recognize bundled packages even when an external library shadows them."
  (cl-letf (((symbol-function #'package-built-in-p)
             (lambda (_name) t))
            ((symbol-function #'locate-library)
             (lambda (_name) "/tmp/external/org.el")))
    (should (neo/builtin-feature-p 'org))))

(ert-deftest neo/builtin-feature-p-recognizes-library-in-emacs-lisp-tree ()
  "Recognize non-package libraries residing in Emacs's Lisp directory."
  (let* ((lisp-root (make-temp-file "neo-emacs-lisp-" t))
         (library (make-temp-file
                   (expand-file-name "neo-builtin-" lisp-root)
                   nil ".el"))
         (lisp-directory lisp-root))
    (unwind-protect
        (cl-letf (((symbol-function #'package-built-in-p)
                   (lambda (_name) nil))
                  ((symbol-function #'locate-library)
                   (lambda (_name) library)))
          (should (neo/builtin-feature-p 'neo-test-builtin)))
      (delete-directory lisp-root t))))

(ert-deftest neo/builtin-feature-p-rejects-external-library ()
  "Do not classify a library outside Emacs's Lisp directory as built-in."
  (let* ((lisp-root (make-temp-file "neo-emacs-lisp-" t))
         (external-root (make-temp-file "neo-external-lisp-" t))
         (library (make-temp-file
                   (expand-file-name "neo-external-" external-root)
                   nil ".el"))
         (lisp-directory lisp-root))
    (unwind-protect
        (cl-letf (((symbol-function #'package-built-in-p)
                   (lambda (_name) nil))
                  ((symbol-function #'locate-library)
                   (lambda (_name) library)))
          (should-not (neo/builtin-feature-p 'neo-test-external)))
      (delete-directory external-root t)
      (delete-directory lisp-root t))))

(ert-deftest neo/builtin-feature-p-rejects-invalid-name ()
  "Signal a type error when NAME is neither a symbol nor a string."
  (should-error (neo/builtin-feature-p 42)
                :type 'wrong-type-argument))

(ert-deftest neo/use-package-adds-ensure-by-default ()
  "Default Neo package declarations to an asynchronous Elpaca ensure."
  (let ((load-file-name neo-use-package-test--file))
    (let ((expansion (prin1-to-string
                      (macroexpand-1 '(neo/use-package sample-package)))))
      (should (string-match-p ":ensure t" expansion))
      (should-not (string-match-p ":wait" expansion)))))

(ert-deftest neo/use-package-immediate-mode-waits-once ()
  "Keep non-extension development evaluation synchronous."
  (let ((load-file-name neo-use-package-test--file)
        (neo/use-extensions nil))
    (let ((expansion (prin1-to-string
                      (macroexpand-1 '(neo/use-package sample-package)))))
      (should (string-match-p ":ensure t" expansion))
      (should (string-match-p "elpaca-wait" expansion)))))

(ert-deftest neo/use-package-keeps-emacs-unensured ()
  "Keep built-in Emacs declarations out of Elpaca."
  (let ((load-file-name neo-use-package-test--file))
    (let ((expansion (prin1-to-string
                      (macroexpand-1 '(neo/use-package emacs :config (ignore))))))
      (should (string-match-p ":ensure nil" expansion)))))

(ert-deftest neo/use-package-keeps-built-in-package-unensured ()
  "Keep packages bundled with a fresh Emacs installation out of Elpaca."
  (let ((load-file-name neo-use-package-test--file))
    (cl-letf (((symbol-function #'package-built-in-p)
               (lambda (_name) t)))
      (let ((expansion
             (prin1-to-string
              (macroexpand-1 '(neo/use-package bundled-package)))))
        (should (string-match-p ":ensure nil" expansion))
        (should-not (string-match-p ":ensure t" expansion))))))

(ert-deftest neo/use-package-preserves-explicit-ensure ()
  "Preserve an explicit `:ensure' recipe without applying the default."
  (let ((load-file-name neo-use-package-test--file))
    (let ((expansion
           (prin1-to-string
            (macroexpand-1
             '(neo/use-package sample-package
                :ensure (:host github :repo "owner/sample-package"))))))
      (should (string-match-p
               ":ensure (:host github :repo \"owner/sample-package\")"
               expansion)))))

(ert-deftest neo/use-package-normalizes-builtin-to-ensure-nil ()
  "Preserve `:builtin' semantics by normalizing it to `:ensure nil'."
  (let ((load-file-name neo-use-package-test--file))
    (let ((expansion
           (prin1-to-string
            (macroexpand-1 '(neo/use-package sample-package :builtin)))))
      (should (string-match-p ":ensure nil" expansion))
      (should-not (string-match-p ":builtin" expansion)))))

(ert-deftest neo/use-package-ensures-system-packages-before-queueing ()
  "Run system prerequisites before storing the replayable declaration."
  (let ((load-file-name neo-use-package-test--file))
    (let ((expansion
           (macroexpand-1
            '(neo/use-package native-package
               :ensure-system-package
               ((pkgconf . pkgconf)
                (("pkgconf" "--exists" "native-library") . native-library-dev))
               :config
               (native-package-mode 1)))))
      (should (eq (car expansion) 'progn))
      (should (eq (car (cadr expansion)) 'neo/ensure-system-packages))
      (should (eq (car (caddr expansion)) 'setq))
      (should-not (memq :ensure-system-package
                        (flatten-tree (caddr expansion)))))))

(ert-deftest neo/ensure-system-packages-does-nothing-when-satisfied ()
  "Do not invoke the installer for requirements already present."
  (let (installed)
    (cl-letf (((symbol-function 'neo--system-package-requirement-satisfied-p)
               (lambda (_check) t))
              ((symbol-function 'neo--install-system-package)
               (lambda (package) (push package installed))))
      (neo/ensure-system-packages '((pkgconf . pkgconf))))
    (should-not installed)))

(ert-deftest neo/ensure-system-packages-installs-before-rechecking ()
  "Install a missing requirement synchronously, then verify it again."
  (let ((check-count 0)
        events)
    (cl-letf (((symbol-function 'neo--system-package-requirement-satisfied-p)
               (lambda (_check)
                 (push 'check events)
                 (> (cl-incf check-count) 1)))
              ((symbol-function 'neo--install-system-package)
               (lambda (_package) (push 'install events))))
      (neo/ensure-system-packages '((native-tool . native-tool-package))))
    (should (equal (nreverse events) '(check install check)))))

(ert-deftest neo/system-package-command-check-runs-synchronously ()
  "Use a direct process call for command-based prerequisite checks."
  (let (invocation)
    (cl-letf (((symbol-function 'executable-find)
               (lambda (command)
                 (and (string= command "pkgconf") "/usr/bin/pkgconf")))
              ((symbol-function 'process-file)
               (lambda (program infile destination display &rest arguments)
                 (setq invocation
                       (list program infile destination display arguments))
                 0)))
      (should
       (neo--system-package-requirement-satisfied-p
        '("pkgconf" "--exists" "enchant-2"))))
    (should
     (equal invocation
            '("/usr/bin/pkgconf" nil nil nil ("--exists" "enchant-2"))))))

(ert-deftest neo/system-package-install-command-uses-apt-synchronously ()
  "Construct the Debian or Ubuntu installer without a shell command."
  (let ((system-type 'gnu/linux))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (command)
                 (pcase command
                   ("apt-get" "/usr/bin/apt-get")
                   ("sudo" "/usr/bin/sudo"))))
              ((symbol-function 'user-uid) (lambda () 1000)))
      (should
       (equal
        (neo--system-package-install-command 'libenchant-2-dev)
        '("/usr/bin/sudo" "--non-interactive" "/usr/bin/apt-get"
          "install" "-y" "libenchant-2-dev"))))))

(ert-deftest neo/system-package-install-command-rejects-unsupported-systems ()
  "Fail clearly rather than guessing a package manager."
  (let ((system-type 'darwin))
    (cl-letf (((symbol-function 'executable-find) (lambda (_command) nil)))
      (should-error
       (neo--system-package-install-command 'libenchant-2-dev)
       :type 'error))))

(ert-deftest neo/install-system-package-reports-command-failure ()
  "Surface synchronous installer output without opening its buffer."
  (cl-letf (((symbol-function 'neo--system-package-install-command)
             (lambda (_package)
               '("/usr/bin/sudo" "/usr/bin/apt-get"
                 "install" "-y" "native-library-dev")))
            ((symbol-function 'process-file)
             (lambda (_program _infile destination _display &rest _args)
               (with-current-buffer destination
                 (insert "permission denied"))
               1)))
    (let ((failure
           (should-error
            (neo--install-system-package 'native-library-dev)
            :type 'error)))
      (should (string-match-p "permission denied"
                              (error-message-string failure))))))

(ert-deftest neo/prepare-use-package-form-disables-duplicate-installs ()
  "Avoid re-queueing duplicate package installs during replay."
  (let ((seen (make-hash-table :test 'equal)))
    (neo--prepare-use-package-form '(use-package sample-package :ensure t) seen)
    (let ((expansion (prin1-to-string
                      (neo--prepare-use-package-form
                       '(use-package sample-package :ensure t :config (ignore))
                       seen))))
      (should (string-match-p ":ensure nil" expansion)))))

(ert-deftest neo/merge-use-package-declarations-disjoint-sections ()
  "Merge disjoint sections from two sources without conflicts."
  (let* ((source-a (cons "neo" "extension-a"))
         (source-b (cons "neo" "extension-b"))
         (pairs (list (cons source-a (neo--sectioned-list->alist '(:init (foo-init))))
                      (cons source-b (neo--sectioned-list->alist '(:config (foo-config))))))
         (provenance (neo--merge-use-package-declarations 'foo pairs)))
    (should (equal (cdr (assoc :init (neo-package-provenance-merged-args-alist provenance)))
                   '((foo-init))))
    (should (equal (cdr (assoc :config (neo-package-provenance-merged-args-alist provenance)))
                   '((foo-config))))
    (should (null (neo-package-provenance-conflicts provenance)))))

(ert-deftest neo/merge-use-package-declarations-custom-same-value-dedupes ()
  "Same `:custom' variable/value pair from two sources dedupes without conflict."
  (let* ((source-a (cons "neo" "questionable-defaults"))
         (source-b (cons "neo" "compsel"))
         (pairs (list (cons source-a (neo--sectioned-list->alist
                                       '(:custom (read-extended-command-predicate #'identity))))
                      (cons source-b (neo--sectioned-list->alist
                                       '(:custom (read-extended-command-predicate #'identity))))))
         (provenance (neo--merge-use-package-declarations 'emacs pairs)))
    (should (equal (cdr (assoc :custom (neo-package-provenance-merged-args-alist provenance)))
                   '((read-extended-command-predicate #'identity))))
    (should (null (neo-package-provenance-conflicts provenance)))))

(ert-deftest neo/merge-use-package-declarations-custom-conflicting-value ()
  "Differing `:custom' values for the same variable keep the first and warn."
  (let* ((source-a (cons "neo" "extension-a"))
         (source-b (cons "neo" "extension-b"))
         (pairs (list (cons source-a (neo--sectioned-list->alist '(:custom (some-var 1))))
                      (cons source-b (neo--sectioned-list->alist '(:custom (some-var 2))))))
         (provenance (neo--merge-use-package-declarations 'foo pairs)))
    (should (equal (cdr (assoc :custom (neo-package-provenance-merged-args-alist provenance)))
                   '((some-var 1))))
    (should (= (length (neo-package-provenance-conflicts provenance)) 1))
    (let ((conflict (car (neo-package-provenance-conflicts provenance))))
      (should (eq (neo-merge-conflict-sub-key conflict) 'some-var))
      (should (equal (neo-merge-conflict-kept-value conflict) '(some-var 1)))
      (should (equal (neo-merge-conflict-dropped-value conflict) '(some-var 2)))
      (should (equal (neo-merge-conflict-kept-source conflict) source-a))
      (should (equal (neo-merge-conflict-dropped-source conflict) source-b)))))

(ert-deftest neo/merge-use-package-declarations-ensure-default-vs-real ()
  "A framework-default `:ensure' loses to a real recipe from another source."
  (let* ((source-a (cons "neo" "ai-buddy"))
         (source-b (cons "neo" "terminal"))
         (pairs (list (cons source-a (neo--sectioned-list->alist
                                       '(:ensure (:host github :repo "akermu/emacs-libvterm"))))
                      (cons source-b (neo--sectioned-list->alist
                                       '(:ensure t :custom (vterm-max-scrollback 100000))))))
         (provenance (neo--merge-use-package-declarations 'vterm pairs)))
    (should (equal (cdr (assoc :ensure (neo-package-provenance-merged-args-alist provenance)))
                   '((:host github :repo "akermu/emacs-libvterm"))))
    (should (equal (cdr (assoc :custom (neo-package-provenance-merged-args-alist provenance)))
                   '((vterm-max-scrollback 100000))))
    (should (null (neo-package-provenance-conflicts provenance)))))

(ert-deftest neo/merge-use-package-declarations-ensure-both-default-collapses ()
  "Two default-only `:ensure' declarations collapse into one, no conflict.
Reconstructs the historical omega-45pw `transient' race: both
declarations only carried the framework-injected default `:ensure', so
merging removes the race by construction instead of relying on Elpaca
queue timing."
  (let* ((source-a (cons "neo" "better-git"))
         (source-b (cons "neo" "programming-foundation"))
         (pairs (list (cons source-a (neo--sectioned-list->alist '(:ensure t)))
                      (cons source-b (neo--sectioned-list->alist '(:ensure t)))))
         (provenance (neo--merge-use-package-declarations 'transient pairs)))
    (should (equal (cdr (assoc :ensure (neo-package-provenance-merged-args-alist provenance)))
                   (list neo--use-package-default-ensure)))
    (should (null (neo-package-provenance-conflicts provenance)))))

(ert-deftest neo/merge-use-package-declarations-ensure-conflicting-recipes ()
  "Two differing real `:ensure' recipes keep the first and warn."
  (let* ((source-a (cons "neo" "better-git"))
         (source-b (cons "neo" "programming-foundation"))
         (pairs (list (cons source-a (neo--sectioned-list->alist '(:ensure (:host github :repo "a/a"))))
                      (cons source-b (neo--sectioned-list->alist '(:ensure (:host github :repo "b/b"))))))
         (provenance (neo--merge-use-package-declarations 'transient pairs)))
    (should (equal (cdr (assoc :ensure (neo-package-provenance-merged-args-alist provenance)))
                   '((:host github :repo "a/a"))))
    (should (= (length (neo-package-provenance-conflicts provenance)) 1))
    (let ((conflict (car (neo-package-provenance-conflicts provenance))))
      (should (eq (neo-merge-conflict-section conflict) :ensure))
      (should (equal (neo-merge-conflict-kept-value conflict) '(:host github :repo "a/a")))
      (should (equal (neo-merge-conflict-dropped-value conflict) '(:host github :repo "b/b"))))))

(ert-deftest neo/merge-use-package-declarations-if-single-source-unchanged ()
  "A single source's `:if nil' passes through unchanged, no conflict."
  (let* ((source-a (cons "neo" "extension-a"))
         (pairs (list (cons source-a (neo--sectioned-list->alist '(:if nil :config (foo-config))))))
         (provenance (neo--merge-use-package-declarations 'foo pairs)))
    (should (equal (cdr (assoc :if (neo-package-provenance-merged-args-alist provenance)))
                   '(nil)))
    (should (null (neo-package-provenance-conflicts provenance)))))

(ert-deftest neo/merge-use-package-declarations-disabled-single-source-unchanged ()
  "A single source's bare `:disabled' passes through unchanged, no conflict."
  (let* ((source-a (cons "neo" "extension-a"))
         (pairs (list (cons source-a (neo--sectioned-list->alist '(:disabled :config (foo-config))))))
         (provenance (neo--merge-use-package-declarations 'foo pairs)))
    (should (assoc :disabled (neo-package-provenance-merged-args-alist provenance)))
    (should (null (cdr (assoc :if (neo-package-provenance-merged-args-alist provenance)))))
    (should (null (neo-package-provenance-conflicts provenance)))))

(ert-deftest neo/merge-use-package-declarations-if-same-form-dedupes ()
  "Two sources with the identical `:if' form dedupe to one, no conflict."
  (let* ((source-a (cons "neo" "extension-a"))
         (source-b (cons "neo" "extension-b"))
         (pairs (list (cons source-a (neo--sectioned-list->alist '(:if my-pred)))
                      (cons source-b (neo--sectioned-list->alist '(:if my-pred)))))
         (provenance (neo--merge-use-package-declarations 'foo pairs)))
    (should (equal (cdr (assoc :if (neo-package-provenance-merged-args-alist provenance)))
                   '(my-pred)))
    (should (null (neo-package-provenance-conflicts provenance)))))

(ert-deftest neo/merge-use-package-declarations-if-differing-conditions-and-combines ()
  "Two sources with differing real `:if' conditions AND-combine, no conflict."
  (let* ((source-a (cons "neo" "extension-a"))
         (source-b (cons "neo" "extension-b"))
         (pairs (list (cons source-a (neo--sectioned-list->alist '(:if pred-a)))
                      (cons source-b (neo--sectioned-list->alist '(:if pred-b)))))
         (provenance (neo--merge-use-package-declarations 'foo pairs)))
    (should (equal (cdr (assoc :if (neo-package-provenance-merged-args-alist provenance)))
                   '((and pred-a pred-b))))
    (should (null (neo-package-provenance-conflicts provenance)))))

(ert-deftest neo/merge-use-package-declarations-if-and-unless-unalias-and-combine ()
  "`:if' from one source and `:unless' from another unalias and AND-combine."
  (let* ((source-a (cons "neo" "extension-a"))
         (source-b (cons "neo" "extension-b"))
         (pairs (list (cons source-a (neo--sectioned-list->alist '(:if pred-a)))
                      (cons source-b (neo--sectioned-list->alist '(:unless pred-b)))))
         (provenance (neo--merge-use-package-declarations 'foo pairs)))
    (should (equal (cdr (assoc :if (neo-package-provenance-merged-args-alist provenance)))
                   '((and pred-a (not pred-b)))))
    (should (null (neo-package-provenance-conflicts provenance)))))

(ert-deftest neo/merge-use-package-declarations-if-unconditional-plus-conditional-conflicts ()
  "An unconditional source alongside a conditional one is flagged as overridden."
  (let* ((source-a (cons "neo" "extension-a"))
         (source-b (cons "neo" "extension-b"))
         (pairs (list (cons source-a (neo--sectioned-list->alist '(:config (foo-config))))
                      (cons source-b (neo--sectioned-list->alist '(:if pred-b)))))
         (provenance (neo--merge-use-package-declarations 'foo pairs)))
    (should (equal (cdr (assoc :if (neo-package-provenance-merged-args-alist provenance)))
                   '(pred-b)))
    (should (= (length (neo-package-provenance-conflicts provenance)) 1))
    (let ((conflict (car (neo-package-provenance-conflicts provenance))))
      (should (eq (neo-merge-conflict-section conflict) :if))
      (should (equal (neo-merge-conflict-kept-source conflict) source-b))
      (should (equal (neo-merge-conflict-dropped-source conflict) source-a))
      (should (equal (neo-merge-conflict-dropped-value conflict) t)))))

(ert-deftest neo/merge-use-package-declarations-disabled-plus-unconditional-conflicts ()
  "A bare `:disabled' source overrides an unconditional one, flagged as a conflict."
  (let* ((source-a (cons "neo" "extension-a"))
         (source-b (cons "neo" "extension-b"))
         (pairs (list (cons source-a (neo--sectioned-list->alist '(:disabled)))
                      (cons source-b (neo--sectioned-list->alist '(:config (foo-config))))))
         (provenance (neo--merge-use-package-declarations 'foo pairs)))
    (should (assoc :disabled (neo-package-provenance-merged-args-alist provenance)))
    (should (null (cdr (assoc :if (neo-package-provenance-merged-args-alist provenance)))))
    (should (= (length (neo-package-provenance-conflicts provenance)) 1))
    (let ((conflict (car (neo-package-provenance-conflicts provenance))))
      (should (eq (neo-merge-conflict-section conflict) :if))
      (should (equal (neo-merge-conflict-kept-source conflict) source-a))
      (should (equal (neo-merge-conflict-dropped-source conflict) source-b)))))

(ert-deftest neo/merge-use-package-declarations-disabled-unanimous-no-conflict ()
  "Two sources both declaring bare `:disabled' merge without conflict."
  (let* ((source-a (cons "neo" "extension-a"))
         (source-b (cons "neo" "extension-b"))
         (pairs (list (cons source-a (neo--sectioned-list->alist '(:disabled)))
                      (cons source-b (neo--sectioned-list->alist '(:disabled)))))
         (provenance (neo--merge-use-package-declarations 'foo pairs)))
    (should (assoc :disabled (neo-package-provenance-merged-args-alist provenance)))
    (should (null (neo-package-provenance-conflicts provenance)))))

(ert-deftest neo/merge-use-package-declarations-if-nil-plus-unconditional-not-disabled ()
  "`:if nil' plus an unconditional source stays `:if nil', not bare `:disabled'."
  (let* ((source-a (cons "neo" "extension-a"))
         (source-b (cons "neo" "extension-b"))
         (pairs (list (cons source-a (neo--sectioned-list->alist '(:if nil)))
                      (cons source-b (neo--sectioned-list->alist '(:config (foo-config))))))
         (provenance (neo--merge-use-package-declarations 'foo pairs)))
    (should (equal (cdr (assoc :if (neo-package-provenance-merged-args-alist provenance)))
                   '(nil)))
    (should (null (assoc :disabled (neo-package-provenance-merged-args-alist provenance))))
    (should (= (length (neo-package-provenance-conflicts provenance)) 1))))

(ert-deftest neo/format-package-provenance-does-not-error-on-merged-if ()
  "Formatting provenance for a package with a merged `:if' does not error."
  (let* ((source-a (cons "neo" "extension-a"))
         (source-b (cons "neo" "extension-b"))
         (pairs (list (cons source-a (neo--sectioned-list->alist '(:if pred-a)))
                      (cons source-b (neo--sectioned-list->alist '(:unless pred-b)))))
         (provenance (neo--merge-use-package-declarations 'foo pairs)))
    (should (stringp (neo--format-package-provenance provenance)))))

(ert-deftest neo/collect-package-sources-groups-by-name-in-order ()
  "Group queued `use-package' forms by package name in dependency order."
  (let* ((slug-a (make-neo/extension-slug :publisher "neo" :name "extension-a"))
         (slug-b (make-neo/extension-slug :publisher "neo" :name "extension-b"))
         (installation-a (make-neo/installation :extension-slug slug-a))
         (installation-b (make-neo/installation :extension-slug slug-b))
         (installed-map (make-hash-table :test 'equal)))
    (puthash "neo:extension-a" installation-a installed-map)
    (puthash "neo:extension-b" installation-b installed-map)
    (let* ((enabled-packages
            (list (cons (cons "neo" "extension-a")
                        (list '(use-package foo :init (foo-init-a))))
                  (cons (cons "neo" "extension-b")
                        (list '(use-package foo :init (foo-init-b))
                              '(use-package bar :config (bar-config))))))
           (sorted-slugs (list "neo:extension-a" "neo:extension-b"))
           (grouped (neo--collect-package-sources sorted-slugs installed-map enabled-packages)))
      (should (equal (mapcar #'car grouped) '(foo bar)))
      (let ((foo-sources (mapcar #'car (cdr (assoc 'foo grouped)))))
        (should (equal foo-sources (list (cons "neo" "extension-a") (cons "neo" "extension-b"))))))))

(ert-deftest neo/format-package-provenance-mentions-sources-and-ensure ()
  "Formatted provenance mentions all contributing sources and the winning `:ensure'."
  (let* ((source-a (cons "neo" "ai-buddy"))
         (source-b (cons "neo" "terminal"))
         (pairs (list (cons source-a (neo--sectioned-list->alist
                                       '(:ensure (:host github :repo "akermu/emacs-libvterm"))))
                      (cons source-b (neo--sectioned-list->alist
                                       '(:ensure t :custom (vterm-max-scrollback 100000))))))
         (provenance (neo--merge-use-package-declarations 'vterm pairs))
         (report (neo--format-package-provenance provenance)))
    (should (string-match-p "neo:ai-buddy" report))
    (should (string-match-p "neo:terminal" report))
    (should (string-match-p "akermu/emacs-libvterm" report))))

(ert-deftest neo/format-package-provenance-reports-single-declaration ()
  "An unduplicated package reports that no merge occurred."
  (let* ((source-a (cons "neo" "solo"))
         (pairs (list (cons source-a (neo--sectioned-list->alist '(:config (solo-config))))))
         (provenance (neo--merge-use-package-declarations 'solo-package pairs))
         (report (neo--format-package-provenance provenance)))
    (should (string-match-p "single declaration" report))
    (should (string-match-p "neo:solo" report))))

(provide 'neo-use-package-test)
;;; neo-use-package-test.el ends here
