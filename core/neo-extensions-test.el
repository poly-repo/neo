;;; neo-extensions-test.el --- Tests for Neo extension loading -*- lexical-binding: t -*-

(require 'ert)
(require 'neo-early-init-utils)

(defvar neo/cache-directory temporary-file-directory)
(defvar neo/config-directory temporary-file-directory)
(defvar neo-extensions-test--delayed-hook nil)

(require 'neo-extensions)

;; `neo/extension' reads `neo--extensions-emblem-path' as part of its
;; own macro-expansion-time code (not its expanded output), so unlike
;; `neo--extensions' (already defvar'd with a default in
;; neo-extensions.el), this variable needs a top-level default here
;; too: eager macro-expansion of a test body reaches the nested
;; `neo/extension' call before the test's own `let' has run, so a
;; per-test `let' binding alone comes too late to satisfy it.
(defvar neo--extensions-emblem-path "/nonexistent")

(ert-deftest neo/use-local-extension-sources-p-requires-named-instance ()
  "Only non-default checkout instances should use local extension sources."
  (let ((user-emacs-directory (make-temp-file "neo-user-emacs-" t)))
    (unwind-protect
        (progn
          (make-directory
           (expand-file-name "extensions/extensions/neo" user-emacs-directory)
           t)
          (cl-letf (((symbol-function 'neo/nondefault-emacs-instance-p)
                     (lambda () t)))
            (should (neo/use-local-extension-sources-p)))
          (cl-letf (((symbol-function 'neo/nondefault-emacs-instance-p)
                     (lambda () nil)))
            (should-not (neo/use-local-extension-sources-p))))
      (delete-directory user-emacs-directory t))))

(ert-deftest neo/local-registry-override-follows-publisher-layout ()
  "Resolve local registry overrides from the checkout layout."
  (let ((user-emacs-directory (make-temp-file "neo-user-emacs-" t))
        expected)
    (unwind-protect
        (progn
          (setq expected
                (expand-file-name "extensions/extensions/mav"
                                  user-emacs-directory))
          (make-directory expected t)
          (cl-letf (((symbol-function 'neo/use-local-extension-sources-p)
                     (lambda () t)))
            (should (equal (neo--local-registry-override "mav")
                           expected))))
      (delete-directory user-emacs-directory t))))

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

(ert-deftest neo/load-extension-keeps-load-path-for-deferred-require ()
  "Keep extension subdirectories available after the top-level load ends."
  (let* ((base-dir (make-temp-file "neo-extension-base-" t))
         (extension-dir (expand-file-name "neo/sample" base-dir))
         (nested-dir (expand-file-name "nested" extension-dir))
         (feature 'neo-extensions-test-delayed-lib)
         (feature-name (symbol-name feature))
         (nested-file (expand-file-name (format "%s.el" feature-name) nested-dir))
         (entry-file (expand-file-name "neo-sample.el" extension-dir))
         (extension (make-neo/extension :publisher "neo" :name "sample"))
         (original-load-path load-path)
         (neo-extensions-test--delayed-hook nil))
    (unwind-protect
        (progn
          (make-directory nested-dir t)
          (with-temp-file nested-file
            (insert (format ";;; -*- lexical-binding: t -*-\n(provide '%s)\n"
                            feature-name)))
          (with-temp-file entry-file
            (insert (format
                     ";;; -*- lexical-binding: t -*-\n(defun neo--sample-delayed-require ()\n  (require '%s))\n(add-hook 'neo-extensions-test--delayed-hook #'neo--sample-delayed-require)\n"
                     feature-name)))
          (cl-letf (((symbol-function 'neo--extensions-base-dir)
                     (lambda () base-dir))
                    ((symbol-function 'neo/use-local-extension-sources-p)
                     (lambda () t)))
            (should (neo--load-extension extension))
            (should (member nested-dir load-path))
            (run-hooks 'neo-extensions-test--delayed-hook)
            (should (featurep feature))))
      (setq load-path original-load-path)
      (setq neo-extensions-test--delayed-hook nil)
      (when (featurep feature)
        (unload-feature feature t))
      (fmakunbound 'neo--sample-delayed-require)
      (delete-directory base-dir t))))

(ert-deftest neo/latest-registry-release-parses-github-assets ()
  "Resolve the published manifest SHA from the latest release assets."
  (let* ((sha "1234567890abcdef1234567890abcdef12345678")
         (manifest-name (format "extensions-%s.el" sha))
         (checksum-name (format "%s.sha256" manifest-name))
         (manifest-url (format "https://example.invalid/%s" manifest-name))
         (checksum-url (format "https://example.invalid/%s" checksum-name))
         (registry
          (make-neo--extension-registry
           :name "mav"
           :url "https://github.com/poly-repo/mav-extensions.git"))
         (response-buffer (generate-new-buffer " *neo-release-response*"))
         (neo--registry-release-cache (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (with-current-buffer response-buffer
            (insert
             "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n"
             (json-encode
              `((assets . [((name . "notes.txt")
                            (browser_download_url . "https://example.invalid/notes.txt"))
                           ((name . ,manifest-name)
                            (browser_download_url . ,manifest-url))
                           ((name . ,checksum-name)
                            (browser_download_url . ,checksum-url))]))))
            (setq-local url-http-response-status 200)
            (setq-local url-http-end-of-headers
                        (save-excursion
                          (goto-char (point-min))
                          (search-forward "\r\n\r\n"))))
          (cl-letf (((symbol-function 'url-retrieve-synchronously)
                     (lambda (&rest _args)
                       response-buffer)))
            (let ((release (neo--latest-registry-release registry)))
              (should (equal (neo--registry-release-sha release) sha))
              (should (equal (neo--registry-release-manifest-url release)
                             manifest-url))
              (should (equal (neo--registry-release-checksum-url release)
                             checksum-url)))))
      (when (buffer-live-p response-buffer)
        (kill-buffer response-buffer)))))

(ert-deftest neo/fetch-extensions-uses-published-release-sha ()
  "Fetch the manifest and content using the published release SHA."
  (let* ((cache-root (make-temp-file "neo-extensions-cache-" t))
         (sha "abcdef1234567890abcdef1234567890abcdef12")
         (manifest-url (format "https://example.invalid/extensions-%s.el" sha))
         (checksum-url (format "%s.sha256" manifest-url))
         (registry
          (make-neo--extension-registry
           :name "mav"
           :url "https://github.com/poly-repo/mav-extensions.git"))
         copied-urls
         downloaded-content-sha)
    (unwind-protect
        (cl-letf (((symbol-function 'neo/get-emacs-instance-name)
                   (lambda ()
                     "neo"))
                  ((symbol-function 'neo--latest-registry-release)
                   (lambda (_registry)
                     (make-neo--registry-release
                      :sha sha
                      :manifest-url manifest-url
                      :checksum-url checksum-url)))
                  ((symbol-function 'neo/cache-file-path)
                   (lambda (path)
                     (expand-file-name path cache-root)))
                  ((symbol-function 'url-copy-file)
                   (lambda (source target &optional _ok-if-exists _keep-time)
                     (push source copied-urls)
                     (with-temp-file target
                       (insert (if (string-suffix-p ".sha256" target)
                                   "checksum"
                                 ";;; -*- lexical-binding: t -*-\n")))
                     target))
                  ((symbol-function 'neo/download-registry-content)
                   (lambda (_registry commit-sha)
                     (setq downloaded-content-sha commit-sha)
                     (let ((content-dir (expand-file-name commit-sha cache-root)))
                       (make-directory content-dir t)
                       content-dir))))
          (let* ((target-file (neo/fetch-extensions registry))
                 (cache-dir (expand-file-name "extensions/mav/" cache-root))
                 (manifest-link (expand-file-name "extensions-current.el"
                                                  cache-dir)))
            (should (equal (file-name-nondirectory target-file)
                           (format "extensions-%s.el" sha)))
            (should (equal downloaded-content-sha sha))
            (should (member manifest-url copied-urls))
            (should (member checksum-url copied-urls))
            (should (file-symlink-p manifest-link))
            (should (equal (file-symlink-p manifest-link)
                           (format "extensions-%s.el" sha)))))
      (delete-directory cache-root t))))

(ert-deftest neo/extension-tree-sitter-grammars-normalizes-single-tuple ()
  "A single (LANG URL ...) tuple is wrapped into a one-element list.
Unlike `:requires', whose single-value case is a bare string, a
single grammar tuple is itself a list, so the macro must distinguish
it from a list of tuples by checking whether the first element is a
list."
  (let ((neo--extensions (make-hash-table :test 'equal))
        (neo--extensions-emblem-path "/nonexistent"))
    (neo/extension
     :name "single-grammar"
     :publisher "neo"
     :description "d"
     :tree-sitter-grammars (haskell "https://example.invalid/haskell" "v0.23.1"))
    (should (equal (neo/extension-tree-sitter-grammars
                    (gethash "neo:single-grammar" neo--extensions))
                   '((haskell "https://example.invalid/haskell" "v0.23.1"))))))

(ert-deftest neo/extension-tree-sitter-grammars-keeps-tuple-list ()
  "A list of tuples is stored as-is (no double-wrapping)."
  (let ((neo--extensions (make-hash-table :test 'equal))
        (neo--extensions-emblem-path "/nonexistent"))
    (neo/extension
     :name "many-grammars"
     :publisher "neo"
     :description "d"
     :tree-sitter-grammars ((bash "https://example.invalid/bash")
                            (c "https://example.invalid/c")))
    (should (equal (neo/extension-tree-sitter-grammars
                    (gethash "neo:many-grammars" neo--extensions))
                   '((bash "https://example.invalid/bash")
                     (c "https://example.invalid/c"))))))

(ert-deftest neo/extension-tree-sitter-grammars-defaults-to-nil ()
  "Extensions that declare no grammars get a nil slot, not an error."
  (let ((neo--extensions (make-hash-table :test 'equal))
        (neo--extensions-emblem-path "/nonexistent"))
    (neo/extension
     :name "no-grammars"
     :publisher "neo"
     :description "d")
    (should-not (neo/extension-tree-sitter-grammars
                 (gethash "neo:no-grammars" neo--extensions)))))

(provide 'neo-extensions-test)
;;; neo-extensions-test.el ends here
