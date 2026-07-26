;;; neo-packages-test.el --- Tests for Neo package replay -*- lexical-binding: t -*-

(require 'ert)

;; `neo-packages.el' only needs the Elpaca feature to satisfy its top-level
;; require for these replay-unit tests; no package-manager bootstrap is needed.
(provide 'neo-elpaca)

(unless (featurep 'dash)
  (defun -partition-before-pred (pred list)
    "Partition LIST before items matching PRED."
    (let (result current)
      (dolist (item list)
        (when (and current (funcall pred item))
          (push (nreverse current) result)
          (setq current nil))
        (push item current))
      (when current
        (push (nreverse current) result))
      (nreverse result)))
  (provide 'dash))

(defmacro use-package (&rest _args)
  "Ignore package configuration while loading replay code for unit tests."
  nil)

(require 'neo-early-init-utils)
(defvar neo/cache-directory temporary-file-directory)
(defvar neo/config-directory temporary-file-directory)
(require 'neo-extensions)

(require 'neo-packages)

(ert-deftest neo/replay-extension-packages-waits-once-after-all-forms ()
  "Keep hot extension replay synchronous with one aggregate wait."
  (let* ((slug (make-neo/extension-slug :publisher "neo" :name "sample"))
         (neo--enabled-packages
          '((("neo" . "sample")
             (use-package first-package :ensure t)
             (use-package second-package :ensure t))))
         (neo--replayed-package-installs (make-hash-table :test 'equal))
         (evaluated nil)
         (wait-count 0))
    (cl-letf (((symbol-function 'neo--prepare-use-package-form)
               (lambda (form _seen) form))
              ((symbol-function 'eval)
               (lambda (form &optional _lexical) (push form evaluated)))
              ((symbol-function 'elpaca-wait)
               (lambda () (cl-incf wait-count))))
      (neo/replay-extension-packages slug))
    (should (= wait-count 1))
    (should (equal (nreverse evaluated)
                   '((use-package first-package :ensure t)
                     (use-package second-package :ensure t))))))

(ert-deftest neo/replay-extension-packages-skips-wait-without-matching-forms ()
  "Do not introduce an empty synchronization barrier for another extension."
  (let* ((slug (make-neo/extension-slug :publisher "neo" :name "other"))
         (neo--enabled-packages
          '((("neo" . "sample")
             (use-package sample-package :ensure t))))
         (wait-count 0))
    (cl-letf (((symbol-function 'elpaca-wait)
               (lambda () (cl-incf wait-count))))
      (neo/replay-extension-packages slug))
    (should (zerop wait-count))))

(provide 'neo-packages-test)
;;; neo-packages-test.el ends here
