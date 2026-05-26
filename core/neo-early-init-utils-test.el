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

(provide 'neo-early-init-utils-test)
;;; neo-early-init-utils-test.el ends here
