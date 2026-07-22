;;; neo-config-test.el --- Tests for Neo config DB -*- lexical-binding: t -*-

(require 'ert)
(require 'neo-early-init-utils)

(defvar neo/cache-directory temporary-file-directory)
(defvar neo/config-directory temporary-file-directory)

(require 'neo-config)

(defun neo-config-test--close-db ()
  "Close the current config DB handle when one is open."
  (when (and neo/config-db-handle (sqlitep neo/config-db-handle))
    (sqlite-close neo/config-db-handle))
  (setq neo/config-db-handle nil))

(ert-deftest neo/init-config-db-creates-a-fresh-db ()
  "Create the config DB and parent directory on a fresh profile path."
  (let* ((neo/config-directory (make-temp-file "neo-config-dir-" t))
         (expected-db (expand-file-name "neo.sqlite"
                                        (expand-file-name "current-profile"
                                                          neo/config-directory))))
    (unwind-protect
        (progn
          (neo-config-test--close-db)
          (neo/init-config-db)
          (should (sqlitep neo/config-db-handle))
          (should (file-exists-p expected-db)))
      (neo-config-test--close-db)
      (delete-directory neo/config-directory t))))

(ert-deftest neo/config-db-round-trips-theme-values ()
  "Persist and retrieve a theme value from a fresh config DB."
  (let ((neo/config-directory (make-temp-file "neo-config-dir-" t)))
    (unwind-protect
        (progn
          (neo-config-test--close-db)
          (neo/init-config-db)
          (neo/set-config "theme" "ef-summer")
          (should (equal (neo/get-config "theme") "ef-summer")))
      (neo-config-test--close-db)
      (delete-directory neo/config-directory t))))

(provide 'neo-config-test)
;;; neo-config-test.el ends here
