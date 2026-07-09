;;; neo-framework-test.el --- Tests for Neo framework bootstrap -*- lexical-binding: t -*-

(require 'ert)
(require 'neo-early-init-utils)

(defvar neo/cache-directory temporary-file-directory)
(defvar neo/config-directory temporary-file-directory)

(require 'neo-framework)

(ert-deftest neo/replay-installed-extensions-packages-completes-bootstrap ()
  "Mark package replay completion before running post-bootstrap hooks."
  (let* ((neo/framework-bootstrapped-p nil)
         (hook-ran nil)
         (hook-fn (lambda () (setq hook-ran neo/framework-bootstrapped-p)))
         (framework (make-neo-framework
                     :available-extensions (make-hash-table :test 'equal)
                     :installed-extensions (make-hash-table :test 'equal))))
    (unwind-protect
        (progn
          (add-hook 'neo/after-framework-bootstrap-hook hook-fn)
          (neo/replay-installed-extensions-packages framework)
          (should neo/framework-bootstrapped-p)
          (should hook-ran))
      (remove-hook 'neo/after-framework-bootstrap-hook hook-fn))))

(ert-deftest neo/replay-installed-extensions-packages-respects-dependency-order ()
  "Replay dependency extensions before the extensions that `:require' them.

`installed-extensions' is a hash table, so `maphash' order is not dependency
order.  Insert the dependent (\"pub:b\") before its dependency (\"pub:a\") so a
naive `maphash' walk would replay them out of order, and assert the fix
(topo-sorting via `neo/topo-sort-from-roots') replays \"pub:a\" first."
  (let* ((available (make-hash-table :test 'equal))
         (installed (make-hash-table :test 'equal))
         (ext-a (make-neo/extension :name "a" :publisher "pub" :requires nil))
         (ext-b (make-neo/extension :name "b" :publisher "pub" :requires '("pub:a")))
         (slug-a (make-neo/extension-slug :publisher "pub" :name "a"))
         (slug-b (make-neo/extension-slug :publisher "pub" :name "b"))
         (replayed-order nil))
    (puthash "pub:a" ext-a available)
    (puthash "pub:b" ext-b available)
    ;; Insertion order is deliberately b-then-a: the bug this guards against
    ;; is `maphash' walking `installed' in insertion/hash order instead of
    ;; dependency order.
    (puthash "pub:b" (make-neo/installation :extension-slug slug-b) installed)
    (puthash "pub:a" (make-neo/installation :extension-slug slug-a) installed)
    (cl-letf (((symbol-function 'neo/replay-extension-packages)
               (lambda (slug) (push (neo/extension-slug-to-string slug) replayed-order))))
      (neo/replay-installed-extensions-packages
       (make-neo-framework :available-extensions available
                           :installed-extensions installed)))
    (should (equal (nreverse replayed-order) '("pub:a" "pub:b")))))

(provide 'neo-framework-test)
;;; neo-framework-test.el ends here
