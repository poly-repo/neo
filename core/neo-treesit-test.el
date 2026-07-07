;;; neo-treesit-test.el --- Tests for Neo tree-sitter grammar collection -*- lexical-binding: t -*-

(require 'ert)
(require 'neo-early-init-utils)

(defvar neo/cache-directory temporary-file-directory)
(defvar neo/config-directory temporary-file-directory)

(require 'neo-treesit)

(defun neo-treesit-test--installation (publisher name)
  "Build a `neo/installation' for PUBLISHER:NAME with no other bookkeeping."
  (make-neo/installation
   :extension-slug (make-neo/extension-slug :publisher publisher :name name)))

(defun neo-treesit-test--available (alist)
  "Build an available-extensions hash table from ALIST of (SLUG . EXTENSION)."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (pair alist)
      (puthash (car pair) (cdr pair) table))
    table))

(ert-deftest neo/collect-tree-sitter-grammars-merges-across-extensions ()
  "Union grammars declared by different installed extensions."
  (let* ((haskell-ext (make-neo/extension
                       :tree-sitter-grammars
                       '((haskell "https://example.invalid/haskell" "v0.23.1"))))
         (mlody-ext (make-neo/extension
                     :tree-sitter-grammars
                     '((starlark "https://example.invalid/starlark"))))
         (available (neo-treesit-test--available
                     `(("neo:haskell" . ,haskell-ext)
                       ("neo:mlody" . ,mlody-ext))))
         (installed (list (neo-treesit-test--installation "neo" "haskell")
                          (neo-treesit-test--installation "neo" "mlody"))))
    (should (equal (sort (neo--collect-tree-sitter-grammars installed available)
                         (lambda (a b) (string< (symbol-name (car a)) (symbol-name (car b)))))
                   '((haskell "https://example.invalid/haskell" "v0.23.1")
                     (starlark "https://example.invalid/starlark"))))))

(ert-deftest neo/collect-tree-sitter-grammars-last-writer-wins ()
  "The later installation's tuple wins when two extensions declare the
same language."
  (let* ((first-ext (make-neo/extension
                     :tree-sitter-grammars '((python "https://example.invalid/first"))))
         (second-ext (make-neo/extension
                      :tree-sitter-grammars '((python "https://example.invalid/second"))))
         (available (neo-treesit-test--available
                     `(("neo:first" . ,first-ext)
                       ("neo:second" . ,second-ext))))
         (installed (list (neo-treesit-test--installation "neo" "first")
                          (neo-treesit-test--installation "neo" "second"))))
    (should (equal (neo--collect-tree-sitter-grammars installed available)
                   '((python "https://example.invalid/second"))))))

(ert-deftest neo/collect-tree-sitter-grammars-warns-only-on-real-conflict ()
  "Warn when two extensions disagree on the same language's tuple, but
stay quiet when they merely repeat the same declaration."
  (let* ((dup-a (make-neo/extension
                :tree-sitter-grammars '((bash "https://example.invalid/bash"))))
         (dup-b (make-neo/extension
                :tree-sitter-grammars '((bash "https://example.invalid/bash"))))
         (available (neo-treesit-test--available
                     `(("neo:dup-a" . ,dup-a)
                       ("neo:dup-b" . ,dup-b))))
         (installed (list (neo-treesit-test--installation "neo" "dup-a")
                          (neo-treesit-test--installation "neo" "dup-b")))
         (warnings nil))
    (cl-letf (((symbol-function 'neo/log-warn)
               (lambda (&rest args) (push args warnings))))
      (neo--collect-tree-sitter-grammars installed available)
      (should-not warnings))

    (let* ((conflict-a (make-neo/extension
                        :tree-sitter-grammars '((bash "https://example.invalid/bash"))))
           (conflict-b (make-neo/extension
                       :tree-sitter-grammars '((bash "https://example.invalid/other"))))
           (conflict-available (neo-treesit-test--available
                                `(("neo:conflict-a" . ,conflict-a)
                                  ("neo:conflict-b" . ,conflict-b))))
           (conflict-installed (list (neo-treesit-test--installation "neo" "conflict-a")
                                     (neo-treesit-test--installation "neo" "conflict-b")))
           (conflict-warnings nil))
      (cl-letf (((symbol-function 'neo/log-warn)
                 (lambda (&rest args) (push args conflict-warnings))))
        (should (equal (neo--collect-tree-sitter-grammars conflict-installed conflict-available)
                       '((bash "https://example.invalid/other"))))
        (should (= (length conflict-warnings) 1))
        (should (eq (nth 2 (car conflict-warnings)) 'bash))))))

(ert-deftest neo/collect-tree-sitter-grammars-ignores-orphaned-installations ()
  "Installations whose slug is absent from AVAILABLE-EXTENSIONS are
skipped rather than signaling an error."
  (let* ((available (neo-treesit-test--available nil))
         (installed (list (neo-treesit-test--installation "neo" "missing"))))
    (should-not (neo--collect-tree-sitter-grammars installed available))))

(ert-deftest neo/collect-tree-sitter-mode-preferences-merges-across-extensions ()
  "Union mode preferences declared by different installed extensions."
  (let* ((haskell-ext (make-neo/extension
                       :tree-sitter-modes
                       '((haskell haskell-mode haskell-ts-mode))))
         (python-ext (make-neo/extension
                      :tree-sitter-modes
                      '((python python-mode python-ts-mode))))
         (available (neo-treesit-test--available
                     `(("neo:haskell" . ,haskell-ext)
                       ("neo:python" . ,python-ext))))
         (installed (list (neo-treesit-test--installation "neo" "haskell")
                          (neo-treesit-test--installation "neo" "python"))))
    (should (equal (sort (neo--collect-tree-sitter-mode-preferences installed available)
                         (lambda (a b) (string< (symbol-name (car a)) (symbol-name (car b)))))
                   '((haskell-mode . (haskell . haskell-ts-mode))
                     (python-mode . (python . python-ts-mode)))))))

(ert-deftest neo/collect-tree-sitter-mode-preferences-last-writer-wins ()
  "The later installation's tuple wins when two extensions target the
same classic mode."
  (let* ((first-ext (make-neo/extension
                     :tree-sitter-modes '((haskell haskell-mode haskell-ts-mode))))
         (second-ext (make-neo/extension
                      :tree-sitter-modes '((haskell haskell-mode other-ts-mode))))
         (available (neo-treesit-test--available
                     `(("neo:first" . ,first-ext)
                       ("neo:second" . ,second-ext))))
         (installed (list (neo-treesit-test--installation "neo" "first")
                          (neo-treesit-test--installation "neo" "second"))))
    (should (equal (neo--collect-tree-sitter-mode-preferences installed available)
                   '((haskell-mode . (haskell . other-ts-mode)))))))

(ert-deftest neo/collect-tree-sitter-mode-preferences-warns-only-on-real-conflict ()
  "Warn when two extensions disagree on the target ts-mode for the same
classic mode, but stay quiet when they merely repeat the same
declaration or target different classic modes for the same language."
  (let* ((dup-a (make-neo/extension
                :tree-sitter-modes '((haskell haskell-mode haskell-ts-mode))))
         (dup-b (make-neo/extension
                :tree-sitter-modes '((haskell haskell-mode haskell-ts-mode))))
         (available (neo-treesit-test--available
                     `(("neo:dup-a" . ,dup-a)
                       ("neo:dup-b" . ,dup-b))))
         (installed (list (neo-treesit-test--installation "neo" "dup-a")
                          (neo-treesit-test--installation "neo" "dup-b")))
         (warnings nil))
    (cl-letf (((symbol-function 'neo/log-warn)
               (lambda (&rest args) (push args warnings))))
      (neo--collect-tree-sitter-mode-preferences installed available)
      (should-not warnings))

    (let* ((conflict-a (make-neo/extension
                        :tree-sitter-modes '((haskell haskell-mode haskell-ts-mode))))
           (conflict-b (make-neo/extension
                       :tree-sitter-modes '((haskell haskell-mode other-ts-mode))))
           (conflict-available (neo-treesit-test--available
                                `(("neo:conflict-a" . ,conflict-a)
                                  ("neo:conflict-b" . ,conflict-b))))
           (conflict-installed (list (neo-treesit-test--installation "neo" "conflict-a")
                                     (neo-treesit-test--installation "neo" "conflict-b")))
           (conflict-warnings nil))
      (cl-letf (((symbol-function 'neo/log-warn)
                 (lambda (&rest args) (push args conflict-warnings))))
        (should (equal (neo--collect-tree-sitter-mode-preferences conflict-installed conflict-available)
                       '((haskell-mode . (haskell . other-ts-mode)))))
        (should (= (length conflict-warnings) 1))
        (should (eq (nth 2 (car conflict-warnings)) 'haskell-mode))))))

(ert-deftest neo/collect-tree-sitter-mode-preferences-different-classic-modes-not-a-conflict ()
  "Two extensions targeting the same LANG but different CLASSIC-MODE
packages must not be treated as conflicting, since conflicts are keyed
on CLASSIC-MODE (what `major-mode-remap-alist' is keyed by), not LANG."
  (let* ((ext-a (make-neo/extension
                :tree-sitter-modes '((haskell haskell-mode haskell-ts-mode))))
         (ext-b (make-neo/extension
                :tree-sitter-modes '((haskell literate-haskell-mode haskell-ts-mode))))
         (available (neo-treesit-test--available
                     `(("neo:a" . ,ext-a)
                       ("neo:b" . ,ext-b))))
         (installed (list (neo-treesit-test--installation "neo" "a")
                          (neo-treesit-test--installation "neo" "b")))
         (warnings nil))
    (cl-letf (((symbol-function 'neo/log-warn)
               (lambda (&rest args) (push args warnings))))
      (should (equal (sort (neo--collect-tree-sitter-mode-preferences installed available)
                           (lambda (a b) (string< (symbol-name (car a)) (symbol-name (car b)))))
                     '((haskell-mode . (haskell . haskell-ts-mode))
                       (literate-haskell-mode . (haskell . haskell-ts-mode)))))
      (should-not warnings))))

(defmacro neo-treesit-test--with-framework (installed-extensions available-extensions &rest body)
  "Run BODY with `neo--framework-instance' stubbed to a framework built
from INSTALLED-EXTENSIONS and AVAILABLE-EXTENSIONS."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'neo--framework-instance)
              (lambda ()
                (let ((installed-table (make-hash-table :test 'equal)))
                  (dolist (installation ,installed-extensions)
                    (puthash (neo/extension-slug-to-string
                              (neo/installation-extension-slug installation))
                             installation installed-table))
                  (make-neo-framework
                   :installed-extensions installed-table
                   :available-extensions ,available-extensions)))))
     ,@body))

(ert-deftest neo/treesit-apply-mode-preferences-sets-remap-when-ready ()
  "Remap CLASSIC-MODE to TS-MODE once the grammar is ready and TS-MODE
is loadable."
  (let* ((ext (make-neo/extension
              :tree-sitter-modes '((haskell haskell-mode haskell-ts-mode))))
         (available (neo-treesit-test--available `(("neo:haskell" . ,ext))))
         (installed (list (neo-treesit-test--installation "neo" "haskell")))
         (major-mode-remap-alist nil)
         (neo/treesit-disable-mode-preferences nil))
    (neo-treesit-test--with-framework installed available
      (cl-letf (((symbol-function 'treesit-ready-p)
                 (lambda (lang &optional _quiet) (eq lang 'haskell)))
                ((symbol-function 'fboundp)
                 (lambda (sym) (eq sym 'haskell-ts-mode))))
        (neo/treesit-apply-mode-preferences)
        (should (eq (alist-get 'haskell-mode major-mode-remap-alist)
                    'haskell-ts-mode))))))

(ert-deftest neo/treesit-apply-mode-preferences-clears-remap-when-not-ready ()
  "Clear a stale remap for CLASSIC-MODE when the grammar is not ready."
  (let* ((ext (make-neo/extension
              :tree-sitter-modes '((haskell haskell-mode haskell-ts-mode))))
         (available (neo-treesit-test--available `(("neo:haskell" . ,ext))))
         (installed (list (neo-treesit-test--installation "neo" "haskell")))
         (major-mode-remap-alist '((haskell-mode . haskell-ts-mode)))
         (neo/treesit-disable-mode-preferences nil))
    (neo-treesit-test--with-framework installed available
      (cl-letf (((symbol-function 'treesit-ready-p)
                 (lambda (&rest _args) nil)))
        (neo/treesit-apply-mode-preferences)
        (should-not (alist-get 'haskell-mode major-mode-remap-alist))))))

(ert-deftest neo/treesit-apply-mode-preferences-respects-kill-switch ()
  "Do nothing at all when `neo/treesit-disable-mode-preferences' is
non-nil, even when the grammar is ready."
  (let* ((ext (make-neo/extension
              :tree-sitter-modes '((haskell haskell-mode haskell-ts-mode))))
         (available (neo-treesit-test--available `(("neo:haskell" . ,ext))))
         (installed (list (neo-treesit-test--installation "neo" "haskell")))
         (major-mode-remap-alist nil)
         (neo/treesit-disable-mode-preferences t))
    (neo-treesit-test--with-framework installed available
      (cl-letf (((symbol-function 'treesit-ready-p)
                 (lambda (lang &optional _quiet) (eq lang 'haskell)))
                ((symbol-function 'fboundp)
                 (lambda (sym) (eq sym 'haskell-ts-mode))))
        (neo/treesit-apply-mode-preferences)
        (should-not major-mode-remap-alist)))))

(ert-deftest neo/treesit-apply-mode-preferences-logs-when-overriding-existing-entry ()
  "Log via `neo/log-info' when overwriting a pre-existing remap that
points somewhere else, rather than silently clobbering it."
  (let* ((ext (make-neo/extension
              :tree-sitter-modes '((haskell haskell-mode haskell-ts-mode))))
         (available (neo-treesit-test--available `(("neo:haskell" . ,ext))))
         (installed (list (neo-treesit-test--installation "neo" "haskell")))
         (major-mode-remap-alist '((haskell-mode . some-other-mode)))
         (neo/treesit-disable-mode-preferences nil)
         (info-logs nil))
    (neo-treesit-test--with-framework installed available
      (cl-letf (((symbol-function 'treesit-ready-p)
                 (lambda (lang &optional _quiet) (eq lang 'haskell)))
                ((symbol-function 'fboundp)
                 (lambda (sym) (eq sym 'haskell-ts-mode)))
                ((symbol-function 'neo/log-info)
                 (lambda (&rest args) (push args info-logs))))
        (neo/treesit-apply-mode-preferences)
        (should (eq (alist-get 'haskell-mode major-mode-remap-alist)
                    'haskell-ts-mode))
        (should (= (length info-logs) 1))))))

(provide 'neo-treesit-test)
;;; neo-treesit-test.el ends here
