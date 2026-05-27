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

(provide 'neo-framework-test)
;;; neo-framework-test.el ends here
