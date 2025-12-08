;; devex/editors/emacs/test-early-init.el
(require 'ert)
(load-file "devex/editors/emacs/early-init.el")

;; Simple checks
(ert-deftest early-init-gc-threshold ()
  "Check gc-cons-threshold is set high."
  (should (= gc-cons-threshold (* 100 1000 1000))))

;; Reset the cache between tests
(defun neo/reset-cache ()
  (setq neo/--emacs-instance-name-cache nil))

;;; Test $EMACS_NAME environment variable
(ert-deftest neo/get-emacs-instance-name-uses-env ()
  "Return value from $EMACS_NAME if present."
  (neo/reset-cache)
  (let ((process-environment (cons "EMACS_NAME=testname" process-environment)))
    (should (equal (neo/get-emacs-instance-name) "testname"))))

;;; Test /proc/self/cmdline parsing (Linux only)
(ert-deftest neo/get-emacs-instance-name-uses-proc ()
  "Return name from --name argument in /proc/self/cmdline."
  (neo/reset-cache)
  (when (eq system-type 'gnu/linux)
    (cl-letf (((symbol-function 'file-readable-p) (lambda (_f) t))
              ((symbol-function 'insert-file-contents-literally)
               (lambda (_f)
                 ;; simulate cmdline with "--daemon --name docker-emacs"
                 (insert "--daemon\0--name\0docker-emacs\0"))))
      (should (equal (neo/get-emacs-instance-name) "docker-emacs")))))

;;; Test fallback
(ert-deftest neo/get-emacs-instance-name-fallback ()
  "Return 'neo' if neither EMACS_NAME nor /proc/self/cmdline is available."
  (neo/reset-cache)
  (cl-letf (((symbol-function 'getenv) (lambda (_name) nil))
            ((symbol-function 'file-readable-p) (lambda (_f) nil)))
    (should (equal (neo/get-emacs-instance-name) "neo"))))

;; Run all tests and exit with proper status
(ert-run-tests-batch-and-exit)
