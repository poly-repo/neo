;;; neo-use-package-test.el --- Tests for neo/use-package -*- lexical-binding: t -*-

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

(require 'neo-use-package)

(defconst neo-use-package-test--file
  "/tmp/neo/extensions/extensions/neo/test/neo-test.el")

(defvar neo/use-extensions t)

(ert-deftest neo/use-package-adds-ensure-by-default ()
  "Default Neo package declarations to a synchronous Elpaca ensure."
  (let ((load-file-name neo-use-package-test--file))
    (let ((expansion (prin1-to-string
                      (macroexpand-1 '(neo/use-package sample-package)))))
      (should (string-match-p ":ensure (:wait t)" expansion)))))

(ert-deftest neo/use-package-keeps-emacs-unensured ()
  "Keep built-in Emacs declarations out of Elpaca."
  (let ((load-file-name neo-use-package-test--file))
    (let ((expansion (prin1-to-string
                      (macroexpand-1 '(neo/use-package emacs :config (ignore))))))
      (should (string-match-p ":ensure nil" expansion)))))

(ert-deftest neo/prepare-use-package-form-disables-duplicate-installs ()
  "Avoid re-queueing duplicate package installs during replay."
  (let ((seen (make-hash-table :test 'equal)))
    (neo--prepare-use-package-form '(use-package sample-package :ensure t) seen)
    (let ((expansion (prin1-to-string
                      (neo--prepare-use-package-form
                       '(use-package sample-package :ensure t :config (ignore))
                       seen))))
      (should (string-match-p ":ensure nil" expansion)))))

(provide 'neo-use-package-test)
;;; neo-use-package-test.el ends here
