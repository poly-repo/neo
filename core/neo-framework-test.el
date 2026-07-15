;;; neo-framework-test.el --- Tests for Neo framework bootstrap -*- lexical-binding: t -*-

(require 'ert)
(require 'neo-early-init-utils)

(defvar neo/cache-directory temporary-file-directory)
(defvar neo/config-directory temporary-file-directory)

;; `neo--enabled-packages' is owned by `neo-packages.el', which this file
;; deliberately does not require (see the stubbing note on
;; `neo/replay-installed-extensions-packages-completes-bootstrap' below);
;; pre-declare it so `neo/replay-installed-extensions-packages' has
;; something to read.
(defvar neo--enabled-packages nil)

(require 'neo-framework)

(ert-deftest neo/replay-installed-extensions-packages-completes-bootstrap ()
  "Mark package replay completion before running post-bootstrap hooks.

`neo--collect-package-sources' lives in `neo-packages.el', which this
test file deliberately does not require -- `neo-framework.el' itself
relies on `neo-packages.el' having been loaded earlier in the real boot
sequence (see `core/neo.el's require ordering) rather than requiring it
directly, so tests exercising this method stub the cross-module call
the same way the dependency-order test below does."
  (let* ((neo/framework-bootstrapped-p nil)
         (hook-ran nil)
         (hook-fn (lambda () (setq hook-ran neo/framework-bootstrapped-p)))
         (framework (make-neo-framework
                     :available-extensions (make-hash-table :test 'equal)
                     :installed-extensions (make-hash-table :test 'equal))))
    (unwind-protect
        (cl-letf (((symbol-function 'neo--collect-package-sources)
                   (lambda (_sorted-slugs _installed-map _enabled-packages) nil)))
          (add-hook 'neo/after-framework-bootstrap-hook hook-fn)
          (neo/replay-installed-extensions-packages framework)
          (should neo/framework-bootstrapped-p)
          (should hook-ran))
      (remove-hook 'neo/after-framework-bootstrap-hook hook-fn))))

(ert-deftest neo/replay-installed-extensions-packages-respects-dependency-order ()
  "Feed `neo--collect-package-sources' extensions in dependency order.

`installed-extensions' is a hash table, so `maphash' order is not dependency
order.  Insert the dependent (\"pub:b\") before its dependency (\"pub:a\") so a
naive `maphash' walk would produce a SORTED-SLUGS argument out of order, and
assert the fix (topo-sorting via `neo/topo-sort-from-roots') passes
`neo--collect-package-sources' \"pub:a\" before \"pub:b\"."
  (let* ((available (make-hash-table :test 'equal))
         (installed (make-hash-table :test 'equal))
         (ext-a (make-neo/extension :name "a" :publisher "pub" :requires nil))
         (ext-b (make-neo/extension :name "b" :publisher "pub" :requires '("pub:a")))
         (slug-a (make-neo/extension-slug :publisher "pub" :name "a"))
         (slug-b (make-neo/extension-slug :publisher "pub" :name "b"))
         (captured-slugs nil))
    (puthash "pub:a" ext-a available)
    (puthash "pub:b" ext-b available)
    ;; Insertion order is deliberately b-then-a: the bug this guards against
    ;; is `maphash' walking `installed' in insertion/hash order instead of
    ;; dependency order.
    (puthash "pub:b" (make-neo/installation :extension-slug slug-b) installed)
    (puthash "pub:a" (make-neo/installation :extension-slug slug-a) installed)
    (cl-letf (((symbol-function 'neo--collect-package-sources)
               (lambda (sorted-slugs _installed-map _enabled-packages)
                 (setq captured-slugs sorted-slugs)
                 nil)))
      (neo/replay-installed-extensions-packages
       (make-neo-framework :available-extensions available
                           :installed-extensions installed)))
    (should (equal captured-slugs '("pub:a" "pub:b")))))

(provide 'neo-framework-test)
;;; neo-framework-test.el ends here
