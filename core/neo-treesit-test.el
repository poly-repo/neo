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

(provide 'neo-treesit-test)
;;; neo-treesit-test.el ends here
