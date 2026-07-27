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

(defconst neo-extensions-test--this-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory this test file lives in.
Captured at top level (load time) rather than inside a test body,
since `load-file-name' is only bound while this file is being loaded
— by the time ERT runs an individual test, it is back to nil.")

(ert-deftest neo/refresh-package-archives-skips-work-when-already-loaded ()
  "Do not read disk or contact archives when metadata is already in memory."
  (let ((package-archive-contents '((cached-package . cached-description)))
        (cache-read-p nil)
        (refresh-p nil))
    (cl-letf (((symbol-function 'package-read-all-archive-contents)
               (lambda () (setq cache-read-p t)))
              ((symbol-function 'package-refresh-contents)
               (lambda () (setq refresh-p t))))
      (neo/refresh-package-archives))
    (should-not cache-read-p)
    (should-not refresh-p)))

(ert-deftest neo/refresh-package-archives-prefers-disk-cache ()
  "Use cached metadata without contacting public package archives."
  (let* ((neo/cache-directory (make-temp-file "neo-package-cache-" t))
         (expected-package-dir
          (expand-file-name "elpa-packages" neo/cache-directory))
         (package-archive-contents nil)
         (observed-package-dir nil)
         (refresh-p nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'package-read-all-archive-contents)
                     (lambda ()
                       (setq observed-package-dir package-user-dir
                             package-archive-contents
                             '((cached-package . cached-description)))))
                    ((symbol-function 'package-refresh-contents)
                     (lambda () (setq refresh-p t))))
            (neo/refresh-package-archives))
          (should (equal observed-package-dir expected-package-dir))
          (should-not refresh-p))
      (delete-directory neo/cache-directory t))))

(ert-deftest neo/refresh-package-archives-refreshes-first-use-noninteractively ()
  "Refresh an empty cache noninteractively under Neo's package directory."
  (let* ((neo/cache-directory (make-temp-file "neo-package-cache-" t))
         (expected-package-dir
          (expand-file-name "elpa-packages" neo/cache-directory))
         (package-archive-contents nil)
         (observed-package-dir nil)
         (observed-noninteractive nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'package-read-all-archive-contents)
                     #'ignore)
                    ((symbol-function 'package-refresh-contents)
                     (lambda ()
                       (setq observed-package-dir package-user-dir
                             observed-noninteractive
                             url-request-noninteractive
                             package-archive-contents
                             '((downloaded-package . downloaded-description))))))
            (neo/refresh-package-archives))
          (should (equal observed-package-dir expected-package-dir))
          (should observed-noninteractive))
      (delete-directory neo/cache-directory t))))

(ert-deftest neo/refresh-package-archives-degrades-after-refresh-failure ()
  "Leave metadata empty and log a warning when first-use refresh fails."
  (let ((package-archive-contents nil)
        (warning-message nil))
    (cl-letf (((symbol-function 'package-read-all-archive-contents)
               #'ignore)
              ((symbol-function 'package-refresh-contents)
               (lambda () (error "network unavailable")))
              ((symbol-function 'neo/log-warn)
               (lambda (category format-string &rest args)
                 (setq warning-message
                       (cons category (apply #'format format-string args))))))
      (neo/refresh-package-archives))
    (should-not package-archive-contents)
    (should (equal warning-message
                   '(core . "Could not refresh package archives: network unavailable")))))

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

(ert-deftest neo--get-extension-info-survives-require-of-uninstalled-extension ()
  "Return nil instead of erroring when EXT's entry file requires a feature
owned by an extension that is not (yet) on `load-path'.

Regression test: `neo--get-extension-info' loads every *available*
extension's entry file to introspect its `neo/use-package' calls for the
Extension Manager's card display, even when that extension is not
installed. An extension like neo:neo-workflow does a top-level `(require
'beads-client)' that only resolves once neo:programming-foundation has
been loaded; rendering its card before that must not crash the caller."
  (let* ((base-dir (make-temp-file "neo-extension-base-" t))
         (extension-dir (expand-file-name "neo/sample" base-dir))
         (entry-file (expand-file-name "neo-sample.el" extension-dir))
         (extension (make-neo/extension :publisher "neo" :name "sample"))
         (original-load-path load-path))
    (unwind-protect
        (progn
          (make-directory extension-dir t)
          (with-temp-file entry-file
            (insert ";;; -*- lexical-binding: t -*-\n(require 'neo-extensions-test-nonexistent-feature)\n"))
          (cl-letf (((symbol-function 'neo--extensions-base-dir)
                     (lambda () base-dir))
                    ;; Avoid a real `package-refresh-contents' network call.
                    ((symbol-function 'neo/refresh-package-archives)
                     (lambda ())))
            (should-not (neo--get-extension-info extension))))
      (setq load-path original-load-path)
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
                            (created_at . "2026-07-25T12:00:00Z")
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

(ert-deftest neo/latest-registry-release-selects-newest-complete-pair ()
  "Select the newest manifest with an exact checksum pair."
  (let* ((older-sha "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
         (newer-sha "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb")
         (unpaired-sha "cccccccccccccccccccccccccccccccccccccccc")
         (older-name (format "extensions-%s.el" older-sha))
         (newer-name (format "extensions-%s.el" newer-sha))
         (unpaired-name (format "extensions-%s.el" unpaired-sha))
         (registry
          (make-neo--extension-registry
           :name "neo"
           :url "https://github.com/poly-repo/neo-extensions.git"))
         (neo--registry-release-cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'neo--github-api-get-json)
               (lambda (_url)
                 `((assets
                    . (((name . ,older-name)
                        (created_at . "2026-07-20T12:00:00Z")
                        (browser_download_url . "https://example.invalid/older"))
                       ((name . ,(concat older-name ".sha256"))
                        (browser_download_url . "https://example.invalid/older.sha256"))
                       ((name . ,unpaired-name)
                        (created_at . "2026-07-25T12:00:00Z")
                        (browser_download_url . "https://example.invalid/unpaired"))
                       ((name . ,newer-name)
                        (created_at . "2026-07-24T12:00:00Z")
                        (browser_download_url . "https://example.invalid/newer"))
                       ((name . ,(concat newer-name ".sha256"))
                        (browser_download_url . "https://example.invalid/newer.sha256"))))))))
      (let ((release (neo--latest-registry-release registry)))
        (should (equal (neo--registry-release-sha release) newer-sha))
        (should (equal (neo--registry-release-manifest-url release)
                       "https://example.invalid/newer"))))))

(ert-deftest neo/latest-registry-release-requires-checksum-pair ()
  "Reject a release that has no complete manifest and checksum pair."
  (let* ((sha "dddddddddddddddddddddddddddddddddddddddd")
         (registry
          (make-neo--extension-registry
           :name "neo"
           :url "https://github.com/poly-repo/neo-extensions.git"))
         (neo--registry-release-cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'neo--github-api-get-json)
               (lambda (_url)
                 `((assets
                    . (((name . ,(format "extensions-%s.el" sha))
                        (created_at . "2026-07-25T12:00:00Z")
                        (browser_download_url . "https://example.invalid/manifest"))))))))
      (should-error (neo--latest-registry-release registry)
                    :type 'error))))

(ert-deftest neo/latest-registry-release-can-refresh-cached-metadata ()
  "Refresh release metadata when explicitly requested."
  (let* ((older-sha "eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee")
         (newer-sha "ffffffffffffffffffffffffffffffffffffffff")
         (registry
          (make-neo--extension-registry
           :name "neo"
           :url "https://github.com/poly-repo/neo-extensions.git"))
         (neo--registry-release-cache (make-hash-table :test #'equal))
         (responses
          (list
           `((assets
              . (((name . ,(format "extensions-%s.el" older-sha))
                  (created_at . "2026-07-20T12:00:00Z")
                  (browser_download_url . "https://example.invalid/older"))
                 ((name . ,(format "extensions-%s.el.sha256" older-sha))
                  (browser_download_url . "https://example.invalid/older.sha256")))))
           `((assets
              . (((name . ,(format "extensions-%s.el" newer-sha))
                  (created_at . "2026-07-25T12:00:00Z")
                  (browser_download_url . "https://example.invalid/newer"))
                 ((name . ,(format "extensions-%s.el.sha256" newer-sha))
                  (browser_download_url . "https://example.invalid/newer.sha256"))))))))
    (cl-letf (((symbol-function 'neo--github-api-get-json)
               (lambda (_url)
                 (pop responses))))
      (should (equal (neo--registry-release-sha
                      (neo--latest-registry-release registry))
                     older-sha))
      (should (equal (neo--registry-release-sha
                      (neo--latest-registry-release registry))
                     older-sha))
      (should (equal (neo--registry-release-sha
                      (neo--latest-registry-release registry t))
                     newer-sha)))))

(ert-deftest neo/fetch-extensions-uses-published-release-sha ()
  "Fetch the manifest and content using the published release SHA."
  (let* ((cache-root (make-temp-file "neo-extensions-cache-" t))
         (sha "abcdef1234567890abcdef1234567890abcdef12")
         (manifest-url (format "https://example.invalid/extensions-%s.el" sha))
         (checksum-url (format "%s.sha256" manifest-url))
         (manifest-content ";;; -*- lexical-binding: t -*-\n")
         (manifest-checksum (secure-hash 'sha256 manifest-content))
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
                   (lambda (_registry &optional _refresh)
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
                       (insert (if (string-suffix-p ".sha256" source)
                                   manifest-checksum
                                 manifest-content)))
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

(ert-deftest neo/fetch-extensions-rejects-checksum-mismatch ()
  "Do not activate a downloaded manifest with a bad checksum."
  (let* ((cache-root (make-temp-file "neo-extensions-cache-" t))
         (sha "0123456789abcdef0123456789abcdef01234567")
         (registry
          (make-neo--extension-registry
           :name "mav"
           :url "https://github.com/poly-repo/mav-extensions.git"))
         downloaded-content)
    (unwind-protect
        (cl-letf (((symbol-function 'neo/get-emacs-instance-name)
                   (lambda () "neo"))
                  ((symbol-function 'neo--latest-registry-release)
                   (lambda (_registry &optional _refresh)
                     (make-neo--registry-release
                      :sha sha
                      :manifest-url "https://example.invalid/manifest"
                      :checksum-url "https://example.invalid/checksum")))
                  ((symbol-function 'neo/cache-file-path)
                   (lambda (path)
                     (expand-file-name path cache-root)))
                  ((symbol-function 'url-copy-file)
                   (lambda (source target &optional _ok-if-exists _keep-time)
                     (with-temp-file target
                       (insert (if (string-suffix-p "checksum" source)
                                   (make-string 64 ?0)
                                 "manifest")))
                     target))
                  ((symbol-function 'neo/download-registry-content)
                   (lambda (&rest _args)
                     (setq downloaded-content t)))
                  ((symbol-function 'neo/log-error)
                   (lambda (&rest _args))))
          (let* ((cache-dir (expand-file-name "extensions/mav/" cache-root))
                 (manifest-link
                  (expand-file-name "extensions-current.el" cache-dir)))
            (should-not (neo/fetch-extensions registry))
            (should-not downloaded-content)
            (should-not (file-exists-p manifest-link))))
      (delete-directory cache-root t))))

(ert-deftest neo/download-github-tarfile-retries-after-extraction-failure ()
  "Leave no target directory when extraction fails, so retry can succeed."
  (let* ((cache-root (make-temp-file "neo-extensions-content-" t))
         (target-dir (expand-file-name "content" cache-root))
         (registry
          (make-neo--extension-registry
           :name "neo"
           :url "https://github.com/poly-repo/neo-extensions.git"))
         (attempt 0))
    (unwind-protect
        (cl-letf (((symbol-function 'url-copy-file)
                   (lambda (_source target &optional _ok-if-exists _keep-time)
                     (with-temp-file target
                       (insert "archive"))
                     target))
                  ((symbol-function 'call-process)
                   (lambda (&rest _args)
                     (setq attempt (1+ attempt))
                     (if (= attempt 1)
                         1
                       (with-temp-file
                           (expand-file-name
                            "extension.el"
                            (car (directory-files
                                  cache-root t
                                  "\\`\\.neo-registry-" t)))
                         (insert "extension"))
                       0))))
          (should-error
           (neo--download-github-tarfile
            registry "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" target-dir)
           :type 'error)
          (should-not (file-exists-p target-dir))
          (neo--download-github-tarfile
           registry "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" target-dir)
          (should (file-exists-p
                   (expand-file-name "extension.el" target-dir))))
      (delete-directory cache-root t))))

(ert-deftest neo/fetch-extensions-returns-nil-when-uncached-and-unreachable ()
  "Degrade gracefully instead of signaling when there is no cached
manifest and the remote fetch fails.

Regression test: with nothing cached (fresh instance, first boot,
network down), `neo/fetch-extensions' used to re-signal the original
error out of its `condition-case' handler, which aborts `(require
'neo)' and leaves the user in a half-initialized Emacs. It must
instead log the failure and return nil."
  (let* ((cache-root (make-temp-file "neo-extensions-cache-" t))
         (registry
          (make-neo--extension-registry
           :name "mav"
           :url "https://github.com/poly-repo/mav-extensions.git"))
         errors)
    (unwind-protect
        (cl-letf (((symbol-function 'neo/get-emacs-instance-name)
                   (lambda ()
                     "neo"))
                  ((symbol-function 'neo--latest-registry-release)
                   (lambda (_registry &optional _refresh)
                     (error "[neo] Could not reach GitHub")))
                  ((symbol-function 'neo/cache-file-path)
                   (lambda (path)
                     (expand-file-name path cache-root)))
                  ((symbol-function 'neo/log-error)
                   (lambda (&rest args) (push args errors))))
          (let* ((cache-dir (expand-file-name "extensions/mav/" cache-root))
                 (manifest-link (expand-file-name "extensions-current.el"
                                                  cache-dir)))
            (should-not (neo/fetch-extensions registry))
            (should (= (length errors) 1))
            (should (eq (caar errors) 'core))
            (should-not (file-exists-p manifest-link))))
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

(ert-deftest neo/extension-tree-sitter-modes-normalizes-single-tuple ()
  "A single (LANG CLASSIC-MODE TS-MODE) tuple is wrapped into a
one-element list, mirroring `:tree-sitter-grammars' normalization."
  (let ((neo--extensions (make-hash-table :test 'equal))
        (neo--extensions-emblem-path "/nonexistent"))
    (neo/extension
     :name "single-mode"
     :publisher "neo"
     :description "d"
     :tree-sitter-modes (haskell haskell-mode haskell-ts-mode))
    (should (equal (neo/extension-tree-sitter-modes
                    (gethash "neo:single-mode" neo--extensions))
                   '((haskell haskell-mode haskell-ts-mode))))))

(ert-deftest neo/extension-tree-sitter-modes-keeps-tuple-list ()
  "A list of tuples is stored as-is (no double-wrapping)."
  (let ((neo--extensions (make-hash-table :test 'equal))
        (neo--extensions-emblem-path "/nonexistent"))
    (neo/extension
     :name "many-modes"
     :publisher "neo"
     :description "d"
     :tree-sitter-modes ((haskell haskell-mode haskell-ts-mode)
                         (python python-mode python-ts-mode)))
    (should (equal (neo/extension-tree-sitter-modes
                    (gethash "neo:many-modes" neo--extensions))
                   '((haskell haskell-mode haskell-ts-mode)
                     (python python-mode python-ts-mode))))))

(ert-deftest neo/extension-tree-sitter-modes-defaults-to-nil ()
  "Extensions that declare no mode preferences get a nil slot, not an
error."
  (let ((neo--extensions (make-hash-table :test 'equal))
        (neo--extensions-emblem-path "/nonexistent"))
    (neo/extension
     :name "no-modes"
     :publisher "neo"
     :description "d")
    (should-not (neo/extension-tree-sitter-modes
                 (gethash "neo:no-modes" neo--extensions)))))

(ert-deftest neo/extension-tree-sitter-modes-loads-from-real-haskell-manifest ()
  "The real Haskell manifest.el's :tree-sitter-modes tuple survives
macro-expansion, guarding against tuple-shape drift between this
suite's synthetic structs and the actual declaration."
  (let ((neo--extensions (make-hash-table :test 'equal))
        (neo--extensions-emblem-path "/nonexistent")
        (manifest-path
         (expand-file-name
          "../extensions/extensions/neo/haskell/manifest.el"
          neo-extensions-test--this-directory)))
    (load manifest-path nil 'nomessage)
    (should (equal (neo/extension-tree-sitter-modes
                    (gethash "neo:haskell" neo--extensions))
                   '((haskell haskell-mode haskell-ts-mode))))))

(defun neo-extensions-test--make-local-extension
    (root publisher name &optional entry-file)
  "Create a local extension directory under ROOT.
When ENTRY-FILE is non-nil, also create its main extension file."
  (let ((directory
         (expand-file-name
          (format
           "devex/editors/emacs/extensions/extensions/%s/%s"
           publisher
           name)
          root)))
    (make-directory directory t)
    (when entry-file
      (with-temp-file (expand-file-name (format "neo-%s.el" name) directory)
        (insert ";;; -*- lexical-binding: t -*-\n")))
    directory))

(defmacro neo-extensions-test--with-current-project (root &rest body)
  "Run BODY with Projectile resolving the current project to ROOT."
  (declare (indent 1) (debug t))
  `(cl-letf (((symbol-function 'projectile-project-root)
              (lambda () ,root)))
     ,@body))

(defun neo-extensions-test--framework-with-slugs (&rest slugs)
  "Return a framework whose available extension table contains SLUGS."
  (require 'neo-framework)
  (let ((available (make-hash-table :test #'equal)))
    (dolist (slug slugs)
      (puthash slug t available))
    (make-neo-framework :available-extensions available
                        :installed-extensions
                        (make-hash-table :test #'equal))))

(ert-deftest neo/edit-extension-candidates-merge-framework-and-local-slugs ()
  "Merge, sort, and deduplicate available and local extension slugs."
  (let ((project-root-directory (make-temp-file "neo-edit-project-" t))
        (user-emacs-directory (make-temp-file "neo-edit-init-" t))
        (neo--framework
         (neo-extensions-test--framework-with-slugs
          "neo:remote" "neo:shared" "other:foreign" "neo:Invalid")))
    (unwind-protect
        (neo-extensions-test--with-current-project project-root-directory
          (neo-extensions-test--make-local-extension
           project-root-directory "mav" "personal")
          (neo-extensions-test--make-local-extension
           project-root-directory "neo" "shared")
          (neo-extensions-test--make-local-extension
           project-root-directory "neo" "Invalid")
          (should
           (equal (neo--editable-extension-slugs)
                  '("mav:personal" "neo:remote" "neo:shared"))))
      (delete-directory project-root-directory t)
      (delete-directory user-emacs-directory t))))

(ert-deftest neo/edit-extension-visits-selected-local-entry-file ()
  "Visit project-local source independently of `user-emacs-directory'."
  (let* ((project-root-directory (make-temp-file "neo-edit-project-" t))
         (user-emacs-directory (make-temp-file "neo-edit-init-" t))
         (directory
          (neo-extensions-test--make-local-extension
           project-root-directory "neo" "sample" t))
         (expected-file (expand-file-name "neo-sample.el" directory))
         (neo--framework
          (neo-extensions-test--framework-with-slugs
           "mav:remote" "neo:sample"))
         completion-collection
         completion-require-match
         visited-file)
    (unwind-protect
        (neo-extensions-test--with-current-project project-root-directory
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_prompt collection _predicate require-match
                              &rest _args)
                       (setq completion-collection collection
                             completion-require-match require-match)
                       "neo:sample"))
                    ((symbol-function 'find-file)
                     (lambda (file &rest _args)
                       (setq visited-file file))))
            (neo/edit-extension)
            (should completion-require-match)
            (should (equal completion-collection
                           '("mav:remote" "neo:sample")))
            (should (equal visited-file expected-file))))
      (delete-directory project-root-directory t)
      (delete-directory user-emacs-directory t))))

(ert-deftest neo/edit-extension-errors-before-prompt-without-local-extensions ()
  "Refuse edit and create operations when no local extensions exist."
  (let ((project-root-directory (make-temp-file "neo-edit-project-" t))
        (user-emacs-directory (make-temp-file "neo-edit-init-" t))
        (prompted nil))
    (unwind-protect
        (neo-extensions-test--with-current-project project-root-directory
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _args)
                       (setq prompted t)))
                    ((symbol-function 'read-string)
                     (lambda (&rest _args)
                       (setq prompted t))))
            (should-error (neo/edit-extension) :type 'user-error)
            (should-error (neo/edit-extension '(4)) :type 'user-error)
            (should-not prompted)))
      (delete-directory project-root-directory t)
      (delete-directory user-emacs-directory t))))

(ert-deftest neo/edit-extension-errors-clearly-without-current-project ()
  "Explain that editing needs a current project before inspecting sources."
  (let ((user-emacs-directory (make-temp-file "neo-edit-init-" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'projectile-project-root)
                   (lambda () nil)))
          (let ((error
                 (should-error (neo/edit-extension) :type 'user-error)))
            (should
             (string-match-p "No current Projectile project"
                             (error-message-string error)))))
      (delete-directory user-emacs-directory t))))

(ert-deftest neo/edit-extension-errors-for-downloaded-only-extension ()
  "Refuse to edit an available extension with no local source directory."
  (let ((project-root-directory (make-temp-file "neo-edit-project-" t))
        (user-emacs-directory (make-temp-file "neo-edit-init-" t))
        (neo--framework
         (neo-extensions-test--framework-with-slugs "neo:remote")))
    (unwind-protect
        (neo-extensions-test--with-current-project project-root-directory
          (neo-extensions-test--make-local-extension
           project-root-directory "neo" "local" t)
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _args) "neo:remote")))
            (should-error (neo/edit-extension) :type 'user-error)))
      (delete-directory project-root-directory t)
      (delete-directory user-emacs-directory t))))

(ert-deftest neo/edit-extension-errors-for-missing-local-entry-file ()
  "Refuse to edit a local extension whose main entry file is absent."
  (let ((project-root-directory (make-temp-file "neo-edit-project-" t))
        (user-emacs-directory (make-temp-file "neo-edit-init-" t))
        (neo--framework
         (neo-extensions-test--framework-with-slugs)))
    (unwind-protect
        (neo-extensions-test--with-current-project project-root-directory
          (neo-extensions-test--make-local-extension
           project-root-directory "mav" "incomplete")
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _args) "mav:incomplete")))
            (should-error (neo/edit-extension) :type 'user-error)))
      (delete-directory project-root-directory t)
      (delete-directory user-emacs-directory t))))

(ert-deftest neo/edit-extension-prefix-creates-minimal-scaffold ()
  "Create a project-local scaffold independently of the init directory."
  (let ((project-root-directory (make-temp-file "neo-edit-project-" t))
        (user-emacs-directory (make-temp-file "neo-edit-init-" t))
        (neo--framework
         (neo-extensions-test--framework-with-slugs))
        read-prompt
        visited-file)
    (unwind-protect
        (neo-extensions-test--with-current-project project-root-directory
          (neo-extensions-test--make-local-extension
           project-root-directory "neo" "existing" t)
          (cl-letf (((symbol-function 'read-string)
                     (lambda (prompt &rest _args)
                       (setq read-prompt prompt)
                       "mav:my-tools"))
                    ((symbol-function 'find-file)
                     (lambda (file &rest _args)
                       (setq visited-file file))))
            (neo/edit-extension '(4)))
          (let* ((directory
                  (expand-file-name
                   "devex/editors/emacs/extensions/extensions/mav/my-tools"
                   project-root-directory))
                 (manifest-file (expand-file-name "manifest.el" directory))
                 (entry-file (expand-file-name "neo-my-tools.el" directory))
                 manifest-content
                 entry-content)
            (with-temp-buffer
              (insert-file-contents manifest-file)
              (setq manifest-content (buffer-string)))
            (with-temp-buffer
              (insert-file-contents entry-file)
              (setq entry-content (buffer-string)))
            (should (equal read-prompt
                           "New extension (publisher:name): "))
            (should (equal visited-file entry-file))
            (should (string-match-p ":name \"my-tools\"" manifest-content))
            (should (string-match-p ":title \"My Tools\"" manifest-content))
            (should (string-match-p ":publisher \"mav\"" manifest-content))
            (should
             (string-match-p
              ":url \"https://github.com/poly-repo/mav-extensions.git\""
              manifest-content))
            (should
             (string-match-p
              ":path \"extensions/mav/my-tools\""
              manifest-content))
            (should (string-match-p ":description \"\"" manifest-content))
            (should (string-match-p ":keywords ()" manifest-content))
            (should (string-match-p ":requires ()" manifest-content))
            (should
             (string-prefix-p ";;; -*- lexical-binding: t -*-"
                              entry-content))
            (should-not (string-match-p "^ *(provide " entry-content))))
      (delete-directory project-root-directory t)
      (delete-directory user-emacs-directory t))))

(ert-deftest neo/edit-extension-rejects-invalid-new-slugs ()
  "Reject unsupported publishers and malformed extension names."
  (dolist (slug '("other:sample"
                  "neo:Upper"
                  "neo:two_words"
                  "neo:-leading"
                  "neo:trailing-"
                  "neo:two:parts"
                  "neo:"))
    (should-error (neo--parse-editable-extension-slug slug)
                  :type 'user-error)))

(ert-deftest neo/edit-extension-rejects-exact-existing-slug ()
  "Reject exact conflicts from either available or local extensions."
  (let ((project-root-directory (make-temp-file "neo-edit-project-" t))
        (user-emacs-directory (make-temp-file "neo-edit-init-" t))
        (neo--framework
         (neo-extensions-test--framework-with-slugs "mav:remote")))
    (unwind-protect
        (neo-extensions-test--with-current-project project-root-directory
          (neo-extensions-test--make-local-extension
           project-root-directory "neo" "existing" t)
          (dolist (slug '("neo:existing" "mav:remote"))
            (cl-letf (((symbol-function 'read-string)
                       (lambda (&rest _args) slug)))
              (should-error (neo/edit-extension '(4))
                            :type 'user-error))))
      (delete-directory project-root-directory t)
      (delete-directory user-emacs-directory t))))

(ert-deftest neo/edit-extension-allows-same-name-under-another-publisher ()
  "Treat publisher:name as identity when checking name conflicts."
  (let ((project-root-directory (make-temp-file "neo-edit-project-" t))
        (user-emacs-directory (make-temp-file "neo-edit-init-" t))
        (neo--framework
         (neo-extensions-test--framework-with-slugs))
        visited-file)
    (unwind-protect
        (neo-extensions-test--with-current-project project-root-directory
          (neo-extensions-test--make-local-extension
           project-root-directory "neo" "shared" t)
          (cl-letf (((symbol-function 'read-string)
                     (lambda (&rest _args) "mav:shared"))
                    ((symbol-function 'find-file)
                     (lambda (file &rest _args)
                       (setq visited-file file))))
            (neo/edit-extension '(4)))
          (should
           (equal visited-file
                  (expand-file-name
                   (concat
                    "devex/editors/emacs/extensions/extensions/"
                    "mav/shared/neo-shared.el")
                   project-root-directory))))
      (delete-directory project-root-directory t)
      (delete-directory user-emacs-directory t))))

(ert-deftest neo/edit-extension-cleans-up-after-scaffold-write-failure ()
  "Leave no target or staging directory when scaffold creation fails."
  (let ((project-root-directory (make-temp-file "neo-edit-project-" t))
        (user-emacs-directory (make-temp-file "neo-edit-init-" t))
        (neo--framework
         (neo-extensions-test--framework-with-slugs)))
    (unwind-protect
        (neo-extensions-test--with-current-project project-root-directory
          (neo-extensions-test--make-local-extension
           project-root-directory "neo" "existing" t)
          (cl-letf (((symbol-function 'read-string)
                     (lambda (&rest _args) "neo:broken"))
                    ((symbol-function 'neo--write-extension-scaffold)
                     (lambda (&rest _args)
                       (error "synthetic write failure"))))
            (should-error (neo/edit-extension '(4)) :type 'error))
          (let ((publisher-directory
                 (expand-file-name
                  "devex/editors/emacs/extensions/extensions/neo"
                  project-root-directory)))
            (should-not
             (file-exists-p
              (expand-file-name "broken" publisher-directory)))
            (should-not
             (directory-files publisher-directory nil
                              "\\`\\.broken-"))))
      (delete-directory project-root-directory t)
      (delete-directory user-emacs-directory t))))

(provide 'neo-extensions-test)
;;; neo-extensions-test.el ends here
