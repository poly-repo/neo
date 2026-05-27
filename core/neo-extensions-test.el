;;; neo-extensions-test.el --- Tests for Neo extension loading -*- lexical-binding: t -*-

(require 'ert)
(require 'neo-early-init-utils)

(defvar neo/cache-directory temporary-file-directory)
(defvar neo/config-directory temporary-file-directory)

(require 'neo-extensions)

(ert-deftest neo/extension-load-path-loads-nested-library ()
  "Make nested extension directories available for `require'."
  (let* ((extension-dir (make-temp-file "neo-extension-" t))
         (nested-dir (expand-file-name "nested" extension-dir))
         (feature 'neo-extension-test-nested-lib)
         (feature-name (symbol-name feature))
         (nested-file (expand-file-name (format "%s.el" feature-name) nested-dir))
         (entry-file (expand-file-name "neo-sample.el" extension-dir)))
    (unwind-protect
        (progn
          (make-directory nested-dir t)
          (with-temp-file nested-file
            (insert (format ";;; -*- lexical-binding: t -*-\n(provide '%s)\n"
                            feature-name)))
          (with-temp-file entry-file
            (insert (format ";;; -*- lexical-binding: t -*-\n(require '%s)\n"
                            feature-name)))
          (let ((load-path (append (neo--extension-load-path extension-dir) load-path)))
            (should (member nested-dir load-path))
            (load entry-file nil 'nomessage 'nosuffix)
            (should (featurep feature))))
      (when (featurep feature)
        (unload-feature feature t))
      (delete-directory extension-dir t))))

(provide 'neo-extensions-test)
;;; neo-extensions-test.el ends here
