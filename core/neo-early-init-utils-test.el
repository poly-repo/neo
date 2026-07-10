;;; neo-early-init-utils-test.el --- Tests for early init utilities -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'neo-early-init-utils)

(ert-deftest neo/load-file-ignores-missing-optional-file ()
  "Do not emit noise when an optional file is absent."
  (let ((messages nil)
        (missing-file (make-temp-name
                       (expand-file-name "neo-missing-" temporary-file-directory))))
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (should-not (neo/load-file missing-file t))
      (should-not messages))))

(ert-deftest neo/load-file-reports-optional-load-errors ()
  "Report actual load errors even when failure is allowed."
  (let ((messages nil)
        (bad-file
         (make-temp-file "neo-bad-load-" nil ".el" "(this is not valid elisp)")))
    (unwind-protect
        (cl-letf (((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (push (apply #'format format-string args) messages))))
          (should-not (neo/load-file bad-file t))
          (should (= (length messages) 1))
          (should (string-match-p
                   (regexp-quote (format "neo: error loading %s:" bad-file))
                   (car messages))))
      (delete-file bad-file))))

(ert-deftest neo/disable-customize-persistence-sets-custom-file ()
  "Point `custom-file' at `null-device' in Neo."
  (let ((custom-file "custom.el"))
    (neo/disable-customize-persistence)
    (should (equal custom-file null-device))))

(ert-deftest neo/disable-customize-persistence-blocks-custom-save-all ()
  "Make `custom-save-all' a harmless no-op in batch code paths."
  (let ((messages nil))
    (unwind-protect
        (progn
          (neo/disable-customize-persistence)
          (require 'cus-edit)
          (should (advice-member-p #'neo--custom-save-all-disabled 'custom-save-all))
          (cl-letf (((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (push (apply #'format format-string args) messages))))
            (should-not (custom-save-all))
            (should (equal messages (list neo--customize-disabled-message)))))
      (when (advice-member-p #'neo--custom-save-all-disabled 'custom-save-all)
        (advice-remove 'custom-save-all #'neo--custom-save-all-disabled)))))

(ert-deftest neo/frame-collapse-repair-hooks-install-from-core-alone ()
  "The collapsed-frame repair must be active without loading `neo-ui-frame'.

Regression test: this repair used to live only in the neo:ui extension, so
boots that never load it -- the first-run splash screen and the
extension-manager-only restart from `neo/start-configuration' -- never
installed the repair hooks, letting a collapsed ~200x200px frame go
unrepaired.  `neo-early-init-utils' is required unconditionally by
early-init.el, so its hooks must be present as soon as this file loads,
with no dependency on neo:ui."
  (should (memq #'neo/apply-restored-frame-geometry emacs-startup-hook))
  (should (memq #'neo/ensure-frame-onscreen-and-usable after-make-frame-functions))
  (should (memq #'neo/save-initial-frame-properties kill-emacs-hook))
  (should (fboundp 'neo--repair-collapsed-frame))
  (should (not (featurep 'neo-ui-frame))))

(provide 'neo-early-init-utils-test)
;;; neo-early-init-utils-test.el ends here
