;;; neo-http-cache-flat.el --- ETag + SHA256 cache (flat layout) -*- lexical-binding: t; -*-

(require 'request)
(require 'subr-x)
(require 'url-parse)

(defun neo/_url-basename (url)
  "Return the filename component of URL (the part after last '/')."
  (let ((p (url-filename (url-generic-parse-url url))))
    (file-name-nondirectory (or p ""))))

(defun neo/_cache-paths-flat (cache-dir url)
  "Return an alist of paths for URL in CACHE-DIR (flat layout).
Keys: data, etag, sha, alt-sha.
Data and sidecars live directly under CACHE-DIR."
  (let* ((basename (neo/_url-basename url))
         (data (expand-file-name basename cache-dir))
         (etag (expand-file-name (concat basename ".etag") cache-dir))
         ;; primary sha file uses the full basename (e.g. thing.tar.gz.sha256)
         (sha  (expand-file-name (concat basename ".sha256") cache-dir))
         ;; alt sha allows basename without suffix (e.g. thing.sha256)
         (basename-noext (file-name-sans-extension basename))
         (alt-sha (expand-file-name (concat basename-noext ".sha256") cache-dir)))
    `((data . ,data) (etag . ,etag) (sha . ,sha) (alt-sha . ,alt-sha) (basename . ,basename))))

(defun neo/_read-file-trim (path)
  "Return contents of PATH as string trimmed, or nil if file doesn't exist."
  (when (file-exists-p path)
    (with-temp-buffer
      (insert-file-contents path)
      (string-trim (buffer-string)))))

(defun neo/_write-file (path content)
  "Write CONTENT (string) to PATH, creating parent dir as needed."
  (make-directory (file-name-directory path) t)
  (with-temp-file path
    (insert content)))

(defun neo/_compute-sha-file (path)
  "Compute SHA256 hex of file at PATH, or nil if file missing."
  (when (file-exists-p path)
    (with-temp-buffer
      (insert-file-contents-literally path)
      (secure-hash 'sha256 (current-buffer)))))

(defun neo/_find-sidecar-sha-flat (paths)
  "Check for any existing sidecar SHA file (sha or alt-sha) in PATHS and return its content (trimmed) or nil."
  (or (neo/_read-file-trim (alist-get 'sha paths))
      (neo/_read-file-trim (alist-get 'alt-sha paths))))

;;;###autoload
(defun neo/fetch-url-with-etag-and-sha (url cache-dir &optional expected-sha force)
  "Fetch URL using ETag caching and SHA256 integrity, storing files flat under CACHE-DIR.

URL: remote URL to fetch.
CACHE-DIR: directory where files are stored directly (caller must ensure no basename collisions).
EXPECTED-SHA (optional): hex SHA256 string that the content must match.
FORCE (optional): non-nil to ignore ETag and re-download.

Returns a plist on success:
  (:data PATH :etag ETAG-STRING-or-nil :sha SHA256-HEX :from-cache t|nil :status HTTP-STATUS)

Signals on failure."
  (unless (and (stringp url) (stringp cache-dir))
    (error "url and cache-dir must be strings"))
  ;; ensure cache-dir exists
  (make-directory cache-dir t)
  (let* ((paths (neo/_cache-paths-flat cache-dir url))
         (data-path (alist-get 'data paths))
         (etag-path (alist-get 'etag paths))
         (sha-path (alist-get 'sha paths))
         (alt-sha-path (alist-get 'alt-sha paths))
         (basename (alist-get 'basename paths))
         ;; prefer existing sidecar sha if present (primary or alt)
         (saved-sidecar-sha (neo/_find-sidecar-sha-flat paths))
         (saved-etag (neo/_read-file-trim etag-path))
         (headers (when (and saved-etag (not force))
                    `(("If-None-Match" . ,saved-etag))))
         (result nil))
    ;; helper to compute data sha now (either sidecar or computed from data)
    (cl-labels ((compute-data-sha
                 ()
                 (or (neo/_find-sidecar-sha-flat paths) (neo/_compute-sha-file data-path))))
      ;; synchronous request
      (request
       url
       :headers headers
       :sync t
       :parser 'buffer-string
       :complete (cl-function
                  (lambda (&key response data error &allow-other-keys)
                    (let ((status (request-response-status-code response)))
                      (cond
                       ;; 304 Not Modified: use existing cache; need a sha (sidecar or computed)
                       ((and (null error) (= status 304))
                        (let ((sha (compute-data-sha)))
                          (if sha
                              (setq result (list :data data-path :etag saved-etag :sha sha :from-cache t :status 304))
                            (setq result (list :error "Cache missing or corrupt" :status 304)))))
                       ;; 200 OK (or no explicit status but body present) -> write files
                       ((and (null error) (or (= status 200) (and (null status) data)))
                        (let ((body (or data "")))
                          ;; write data using the preserved basename in CACHE-DIR
                          (neo/_write-file data-path body)
                          ;; write etag sidecar if server provided it
                          (let ((new-etag (request-response-header response "etag")))
                            (when (and new-etag (not (string-empty-p (string-trim new-etag))))
                              (neo/_write-file etag-path (string-trim new-etag))))
                          ;; compute sha and write primary sha sidecar (basename.sha256)
                          (let ((sha (secure-hash 'sha256 body)))
                            (neo/_write-file sha-path sha)
                            ;; result
                            (setq result (list :data data-path :etag (neo/_read-file-trim etag-path) :sha sha :from-cache nil :status status))))))
                       ;; other error
                       (t
                        (setq result (list :error (or (and error (prin1-to-string error)) "HTTP error") :status status))))))
       :error (cl-function
               (lambda (&key error-thrown &allow-other-keys)
                 (setq result (list :error (prin1-to-string error-thrown) :status nil)))))
    ;; request completed
    (unless result
      (error "Request failed without result for %s" url))
    ;; validate result
    (let ((from-cache (plist-get result :from-cache))
          (got-sha  (plist-get result :sha))
          (err (plist-get result :error)))
      (when err
        (error "Fetch error: %s (status %s)" err (plist-get result :status)))
      ;; if cached but no sha yet, try to compute it
      (when (and from-cache (not got-sha))
        (let ((computed (compute-data-sha)))
          (when computed
            (setq got-sha computed)
            (plist-put result :sha got-sha))))
      ;; expected-sha verification
      (when expected-sha
        (unless (and got-sha (string-equal (downcase got-sha) (downcase expected-sha)))
          (if from-cache
              (progn
                (message "[neo] Cached SHA mismatch (have %s, expected %s) — forcing re-download" got-sha expected-sha)
                ;; remove artifacts and retry with force
                (when (file-exists-p data-path) (delete-file data-path))
                (when (file-exists-p sha-path)  (delete-file sha_path))
                (when (file-exists-p alt-sha-path) (delete-file alt-sha-path))
                (when (file-exists-p etag-path) (delete-file etag-path))
                (neo/fetch-url-with-etag-and-sha url cache-dir expected-sha t))
            (error "SHA256 mismatch: expected %s but got %s" expected-sha got-sha)))))
      ;; normalize sha to lowercase if present
      (plist-put result :sha (when (plist-get result :sha) (downcase (plist-get result :sha))))
      result)))

;; ;;; neo-asset.el --- Neo assets (cl-lib only; unified fetch; NAME.sha256) -*- lexical-binding: t; -*-

;; ;; Author: you
;; ;; Package-Requires: ((emacs "27.1") (cl-lib "0.6"))
;; ;; Keywords: convenience, assets, cache
;; ;; SPDX-License-Identifier: MIT

;; ;;; Commentary:
;; ;; - neo/asset (base): :remote may be URL or builder fn.
;; ;;   Verify: if local NAME.sha256 exists -> compute&compare; else existence.
;; ;; - neo/extensions-config (subclass): :remote MUST be a URL string.
;; ;;   Verify: compare REMOTE NAME.sha256 bytes vs cached local NAME.sha256;
;; ;;           if remote sha unavailable -> existence (no hashing).
;; ;; - neo/fetch (unified): best-effort download of REMOTE NAME.sha256 and
;; ;;   cache it locally; refresh payload on FORCE, missing payload, or sha diff.
;; ;; - neo/ensure: if base verify passes but local NAME.sha256 is missing *and*
;; ;;   a remote URL is available, we still call neo/fetch to cache the sidecar.

;; ;;; Code:

;; (require 'cl-lib)
;; (require 'url)        ;; url-copy-file
;; (require 'url-parse)  ;; url-generic-parse-url, url-recreate-url

;; (defgroup neo/assets nil
;;   "Neo asset management."
;;   :group 'tools
;;   :prefix "neo/")

;; (defcustom neo/asset-cache-dir
;;   (expand-file-name "~/.cache/neo/")
;;   "Base directory where assets are stored locally."
;;   :type 'directory)

;; (defcustom neo/asset-default-extension ".el"
;;   "Default extension used when computing a local asset filename from NAME."
;;   :type 'string)

;; (defcustom neo/asset-default-sha-extension ".sha256"
;;   "Extension used for the sidecar checksum file (NAME.sha256)."
;;   :type 'string)

;; (defcustom neo/asset-error-on-missing t
;;   "If non-nil, missing/failed assets cause `neo/ensure` to signal an error."
;;   :type 'boolean)

;; (defcustom neo/asset-verbose-logging nil
;;   "If non-nil, emit progress messages from fetch/verify."
;;   :type 'boolean)

;; (defun neo/log (fmt &rest args)
;;   "Log FMT with ARGS when `neo/asset-verbose-logging` is non-nil."
;;   (when neo/asset-verbose-logging
;;     (apply #'message (concat "[neo] " fmt) args)))

;; 
;; ;;; Data types (struct inheritance)

;; (cl-defstruct (neo/asset
;;                (:copier nil))
;;   "Base asset.

;; Slots:
;; - NAME (string, required)
;; - LOCAL-PATH (string or nil)  – computed if nil: CACHE/NAME + EXTENSION
;; - EXTENSION (string or nil)   – defaults to `neo/asset-default-extension`
;; - REMOTE (string URL or function (lambda (asset) ...))
;; - SHA-FILE (string or nil)    – local cached sha (DIR/NAME.sha256) or computed if nil
;; - VERIFY-PREDICATE (fn or nil) – instance override for verification
;; - ERROR-ON-MISSING (bool or nil) – falls back to `neo/asset-error-on-missing`"
;;   name local-path extension remote sha-file verify-predicate error-on-missing)

;; (cl-defstruct (neo/extensions-config
;;                (:include neo/asset)
;;                (:copier nil))
;;   "Specialized asset with remote .sha256 policy.

;; For this type, :remote MUST be a URL string.
;; Extra slots:
;; - SHA-URL-OVERRIDE (string or nil) – explicit remote .sha256 URL (NAME.sha256)
;; - SHA-SUFFIX (string or nil) – extension to use for NAME-based sha (default `.sha256`)."
;;   sha-url-override
;;   sha-suffix)

;; ;; Constructors
;; (cl-defun neo/make-asset (&rest plist)
;;   (apply #'make-neo/asset plist))

;; (cl-defun neo/make-extensions-config (&key name remote local-path extension sha-file
;;                                            verify-predicate error-on-missing
;;                                            sha-url sha-suffix)
;;   (unless (and (stringp remote) (> (length remote) 0))
;;     (error "[neo/extensions-config] :remote must be a non-empty URL string"))
;;   (make-neo/extensions-config
;;    :name name
;;    :remote remote
;;    :local-path local-path
;;    :extension extension
;;    :sha-file sha-file
;;    :verify-predicate verify-predicate
;;    :error-on-missing error-on-missing
;;    :sha-url-override sha-url
;;    :sha-suffix sha-suffix))

;; 
;; ;;; Helpers

;; (defun neo/ensure-parent-dir (path)
;;   "Ensure parent directory of PATH exists."
;;   (let ((dir (file-name-directory (expand-file-name path))))
;;     (unless (file-directory-p dir)
;;       (make-directory dir t))))

;; (defun neo/file-bytes (file)
;;   "Return FILE contents as raw bytes (string)."
;;   (with-temp-buffer
;;     (let ((coding-system-for-read 'no-conversion))
;;       (insert-file-contents-literally file))
;;     (buffer-string)))

;; ;; (defun neo/file-sha256 (file)
;; ;;   "Return hex SHA256 of FILE as lowercase string."
;; ;;   (with-temp-buffer
;; ;;     (let ((coding-system-for-read 'no-conversion))
;; ;;       (insert-file-contents-literally file))
;; ;;     ;; Return HEX (not raw bytes): BINARY arg = nil
;; ;;     (secure-hash 'sha256 (current-buffer) nil nil nil)))
;; (defun neo/file-sha256 (file)
;;   "Return lowercase hex SHA256 of FILE bytes."
;;   (with-temp-buffer
;;     (let ((coding-system-for-read 'no-conversion))
;;       (insert-file-contents-literally file))
;;     ;; secure-hash default is hex when BINARY is nil (the default).
;;     (downcase (secure-hash 'sha256 (current-buffer)))))


;; (defun neo/read-sha256-from-file (sha-file)
;;   "Read a SHA256 from SHA-FILE (raw hash or `sha256sum`-style line).
;; Return lowercase 64-hex string or nil."
;;   (when (file-exists-p sha-file)
;;     (with-temp-buffer
;;       (let ((coding-system-for-read 'no-conversion))
;;         (insert-file-contents-literally sha-file))
;;       (goto-char (point-min))
;;       (when (re-search-forward "\\b\\([0-9A-Fa-f]\\{64\\}\\)\\b" nil t)
;;         (downcase (match-string 1))))))

;; (defun neo/error-on-missing-p (a)
;;   "Return non-nil if A should error on missing/verify failure."
;;   (let ((x (neo/asset-error-on-missing a)))
;;     (if (null x) neo/asset-error-on-missing x)))

;; 
;; ;;; Local/remote paths

;; (defun neo/asset-local (a)
;;   "Return absolute local payload path for A."
;;   (or (neo/asset-local-path a)
;;       (expand-file-name
;;        (concat (neo/asset-name a)
;;                (or (neo/asset-extension a) neo/asset-default-extension))
;;        neo/asset-cache-dir)))

;; (defun neo/asset-sha-file-path (a)
;;   "Return absolute local path for NAME.sha256 (computed if slot is nil)."
;;   (or (neo/asset-sha-file a)
;;       (expand-file-name
;;        (concat (neo/asset-name a) neo/asset-default-sha-extension)
;;        (file-name-directory (neo/asset-local a)))))

;; (defun neo/asset-exists-p (a)
;;   "Return non-nil if local payload of A exists."
;;   (file-exists-p (neo/asset-local a)))

;; (defun neo/remote-sha-url (a)
;;   "Return remote URL for NAME.sha256 in the SAME DIRECTORY as :remote.
;; Respects :sha-url-override for neo/extensions-config."
;;   (cond
;;    ((neo/extensions-config-p a)
;;     (or (neo/extensions-config-sha-url-override a)
;;         (let* ((remote (neo/asset-remote a))
;;                (suffix (or (neo/extensions-config-sha-suffix a)
;;                            neo/asset-default-sha-extension)))
;;           (and (stringp remote)
;;                (let* ((u (url-generic-parse-url remote))
;;                       (dir (file-name-directory (url-filename u)))
;;                       (fname (concat (neo/asset-name a) suffix)))
;;                  (setf (url-filename u) (concat dir fname))
;;                  (url-recreate-url u))))))
;;    ((stringp (neo/asset-remote a))
;;     (let* ((u (url-generic-parse-url (neo/asset-remote a)))
;;            (dir (file-name-directory (url-filename u)))
;;            (fname (concat (neo/asset-name a) neo/asset-default-sha-extension)))
;;       (setf (url-filename u) (concat dir fname))
;;       (url-recreate-url u)))
;;    (t nil)))

;; 
;; ;;; Generics

;; (cl-defgeneric neo/verify (a)
;;   "Return non-nil if asset A is valid.")

;; (cl-defgeneric neo/fetch (a &optional force)
;;   "Fetch/build A into its local path. Return non-nil on success.")

;; 
;; ;;; Verification

;; (defun neo/asset-default-verify (a)
;;   "Base default verification:
;; - If local NAME.sha256 exists, payload SHA must match it;
;; - Else, payload existence is enough."
;;   (let* ((path (neo/asset-local a))
;;          (sha  (neo/asset-sha-file-path a)))
;;     (and (file-exists-p path)
;;          (let ((expected (neo/read-sha256-from-file sha)))
;;            (if expected
;;                (condition-case nil
;;                    (string-equal expected (neo/file-sha256 path))
;;                  (error nil))
;;              t)))))

;; (cl-defmethod neo/verify ((a neo/asset))
;;   (if-let ((pred (neo/asset-verify-predicate a)))
;;       (funcall pred a)
;;     (neo/asset-default-verify a)))

;; (defun neo/extensions-config-default-verify (a)
;;   "Valid if payload exists and either:
;; - remote NAME.sha256 is available AND equals cached local NAME.sha256 bytes; or
;; - remote sha is unavailable and payload exists (no hashing)."
;;   (let* ((dest      (neo/asset-local a))
;;          (sha-local (neo/asset-sha-file-path a))
;;          (sha-url   (neo/remote-sha-url a)))
;;     (and (file-exists-p dest)
;;          (if (not (stringp sha-url))
;;              t
;;            (let ((tmp (make-temp-file "neo-rc-sha-"))
;;                  (ok  nil))
;;              (unwind-protect
;;                  (setq ok
;;                        (condition-case err
;;                            (progn
;;                              (url-copy-file sha-url tmp t)
;;                              (and (file-exists-p sha-local)
;;                                   (string-equal (neo/file-bytes tmp)
;;                                                 (neo/file-bytes sha-local)))))
;;                          (error
;;                           (neo/log "verify: sha unavailable %s: %s"
;;                                    sha-url (error-message-string err))
;;                           t)))
;;                (ignore-errors (delete-file tmp)))
;;              ok))))

;; (cl-defmethod neo/verify ((a neo/extensions-config))
;;   (if-let ((pred (neo/asset-verify-predicate a)))
;;       (funcall pred a)
;;     (neo/extensions-config-default-verify a)))

;; 
;; ;;; Unified fetch — caches NAME.sha256 and refreshes payload as needed

;; ;; (cl-defmethod neo/fetch ((a neo/asset) &optional force)
;; ;;   "Fetch/build A and cache its remote NAME.sha256 sidecar if present.

;; ;; Steps:
;; ;;   1) Best-effort GET of remote NAME.sha256 (same dir as :remote) to TMP.
;; ;;   2) Decide if payload needs refresh:
;; ;;        FORCE
;; ;;        OR payload missing
;; ;;        OR (remote sha fetched AND (local sha missing OR bytes differ)).
;; ;;      NOTE: Missing local sha while remote sha exists => **download payload**.
;; ;;   3) If needed, refresh payload via URL or builder function.
;; ;;   4) If remote sha was fetched and either payload wasn't needed or refresh
;; ;;      succeeded, save it locally as NAME.sha256 (byte-for-byte)."
;; ;;   (let* ((dest      (neo/asset-local a))
;; ;;          (sha-local (neo/asset-sha-file-path a))
;; ;;          (remote    (neo/asset-remote a))
;; ;;          (sha-url   (neo/remote-sha-url a)))
;; ;;     (neo/ensure-parent-dir dest)
;; ;;     (neo/ensure-parent-dir sha-local)
;; ;;     (let ((tmp-sha (make-temp-file "neo-fetch-sha-"))
;; ;;           (have-remote-sha nil)
;; ;;           (need-payload nil)
;; ;;           (ok t))
;; ;;       (unwind-protect
;; ;;           (progn
;; ;;             ;; 1) Try to fetch the remote sha (optional)
;; ;;             (when (stringp sha-url)
;; ;;               (setq have-remote-sha
;; ;;                     (condition-case err
;; ;;                         (progn
;; ;;                           (url-copy-file sha-url tmp-sha t)
;; ;;                           (neo/log "fetched remote sha: %s" sha-url)
;; ;;                           t)
;; ;;                       (error
;; ;;                        (neo/log "sha unavailable %s: %s" sha-url (error-message-string err))
;; ;;                        nil))))
;; ;;             ;; 2) Decide if we need to refresh payload
;; ;;             (setq need-payload
;; ;;                   (or force
;; ;;                       (not (file-exists-p dest))
;; ;;                       (and have-remote-sha
;; ;;                            (not (and (file-exists-p sha-local)
;; ;;                                      (string-equal (neo/file-bytes tmp-sha)
;; ;;                                                    (neo/file-bytes sha-local)))))))
;; ;;             ;; 3) Refresh payload if needed
;; ;;             (when (and ok need-payload)
;; ;;               (setq ok
;; ;;                     (cond
;; ;;                      ((stringp remote)
;; ;;                       (let ((tmp (make-temp-file "neo-asset-" nil ".tmp"))
;; ;;                             (ok2 nil))
;; ;;                         (unwind-protect
;; ;;                             (setq ok2
;; ;;                                   (condition-case err
;; ;;                                       (progn
;; ;;                                         (url-copy-file remote tmp t)
;; ;;                                         (rename-file tmp dest t)
;; ;;                                         (neo/log "downloaded payload -> %s" dest)
;; ;;                                         t)
;; ;;                                     (error
;; ;;                                      (message "[neo/fetch] payload download failed %s: %s"
;; ;;                                               remote (error-message-string err))
;; ;;                                      nil)))
;; ;;                           (ignore-errors (delete-file tmp)))
;; ;;                         ok2))
;; ;;                      ((functionp remote) ;; base builder
;; ;;                       (condition-case err
;; ;;                           (prog1 (funcall remote a)
;; ;;                             (neo/log "builder created payload -> %s" dest))
;; ;;                         (error
;; ;;                          (message "[neo/fetch] builder failed for %s: %s"
;; ;;                                   (neo/asset-name a) (error-message-string err))
;; ;;                          nil)))
;; ;;                      (t
;; ;;                       (message "[neo/fetch] no remote for %s" (neo/asset-name a))
;; ;;                       nil))))
;; ;;             ;; 4) Save the sha sidecar iff we fetched it and (no refresh needed OR refresh succeeded)
;; ;;             (when (and have-remote-sha (or (not need-payload) ok))
;; ;;               (condition-case err
;; ;;                   (progn
;; ;;                     (copy-file tmp-sha sha-local t t t) ;; overwrite; keep-time; preserve attrs
;; ;;                     (neo/log "saved local sha -> %s" sha-local))
;; ;;                 (error
;; ;;                  (message "[neo/fetch] cannot write local sha %s: %s"
;; ;;                           sha-local (error-message-string err))
;; ;;                  (setq ok nil))))
;; ;;             ok)
;; ;;         (ignore-errors (delete-file tmp-sha))))))

;; ;; (cl-defmethod neo/fetch ((a neo/asset) &optional force)
;; ;;   "Fetch/build A and cache its remote (or computed) NAME.sha256 sidecar.

;; ;; When :remote is a function, it MUST return the payload as a string. We compute
;; ;; its SHA256 and replace the payload iff FORCE, the payload is missing, or the
;; ;; SHA changed. The local NAME.sha256 is always refreshed to the latest value.

;; ;; When :remote is a URL string, we best-effort fetch NAME.sha256 from the same
;; ;; directory, refresh on FORCE/missing/sha-diff, and cache the sidecar."
;; ;;   (let* ((dest      (neo/asset-local a))
;; ;;          (sha-local (neo/asset-sha-file-path a))
;; ;;          (remote    (neo/asset-remote a))
;; ;;          (sha-url   (neo/remote-sha-url a)))
;; ;;     (neo/ensure-parent-dir dest)
;; ;;     (neo/ensure-parent-dir sha-local)
;; ;;     (cond
;; ;;      ;; ---------------------------
;; ;;      ;; Builder function branch
;; ;;      ;; ---------------------------
;; ;;      ((functionp remote)
;; ;;       (let* ((payload
;; ;;               (condition-case err
;; ;;                   (funcall remote a)
;; ;;                 (error
;; ;;                  (message "[neo/fetch] builder failed for %s: %s"
;; ;;                           (neo/asset-name a) (error-message-string err))
;; ;;                  nil))))
;; ;;         (unless (stringp payload)
;; ;;           (message "[neo/fetch] builder for %s did not return a string" (neo/asset-name a))
;; ;;           (cl-return-from neo/fetch nil))
;; ;;         ;; Normalize bytes we will actually write & hash
;; ;;         (let* ((bytes (if (multibyte-string-p payload)
;; ;;                           ;; If builder returned multibyte text, encode deterministically.
;; ;;                           (encode-coding-string payload 'utf-8-unix)
;; ;;                         payload))
;; ;;                (sha-hex (secure-hash 'sha256 bytes))
;; ;;                (current (neo/read-sha256-from-file sha-local))
;; ;;                (need (or force
;; ;;                          (not (file-exists-p dest))
;; ;;                          (not (and current (string-equal current sha-hex))))))
;; ;;           ;; Replace payload only when needed
;; ;;           (when need
;; ;;             (condition-case err
;; ;;                 (let ((coding-system-for-write 'no-conversion))
;; ;;                   (with-temp-file dest
;; ;;                     (set-buffer-multibyte nil)
;; ;;                     (insert bytes)))
;; ;;               (error
;; ;;                (message "[neo/fetch] cannot write payload %s: %s"
;; ;;                         dest (error-message-string err))
;; ;;                (cl-return-from neo/fetch nil))))
;; ;;           ;; Always (re)write local NAME.sha256 to reflect the new content
;; ;;           (condition-case err
;; ;;               (with-temp-file sha-local
;; ;;                 (insert sha-hex) (insert "\n"))
;; ;;             (error
;; ;;              (message "[neo/fetch] cannot write local sha %s: %s"
;; ;;                       sha-local (error-message-string err))
;; ;;              (cl-return-from neo/fetch nil)))
;; ;;           t)))
;; ;;      ;; ---------------------------
;; ;;      ;; URL string branch (unchanged behavior)
;; ;;      ;; ---------------------------
;; ;;      ((stringp remote)
;; ;;       (let ((tmp-sha (make-temp-file "neo-fetch-sha-"))
;; ;;             (have-remote-sha nil)
;; ;;             (need-payload nil)
;; ;;             (ok t))
;; ;;         (unwind-protect
;; ;;             (progn
;; ;;               ;; 1) Try to fetch the remote sha (optional)
;; ;;               (when (stringp sha-url)
;; ;;                 (setq have-remote-sha
;; ;;                       (condition-case err
;; ;;                           (progn
;; ;;                             (url-copy-file sha-url tmp-sha t)
;; ;;                             (neo/log "fetched remote sha: %s" sha-url)
;; ;;                             t)
;; ;;                         (error
;; ;;                          (neo/log "sha unavailable %s: %s" sha-url (error-message-string err))
;; ;;                          nil))))
;; ;;               ;; 2) Decide if we need to refresh payload
;; ;;               (setq need-payload
;; ;;                     (or force
;; ;;                         (not (file-exists-p dest))
;; ;;                         (and have-remote-sha
;; ;;                              (not (and (file-exists-p sha-local)
;; ;;                                        (string-equal (neo/file-bytes tmp-sha)
;; ;;                                                      (neo/file-bytes sha-local)))))))
;; ;;               ;; 3) Refresh payload if needed
;; ;;               (when (and ok need-payload)
;; ;;                 (setq ok
;; ;;                       (let ((tmp (make-temp-file "neo-asset-" nil ".tmp")))
;; ;;                         (unwind-protect
;; ;;                             (condition-case err
;; ;;                                 (progn
;; ;;                                   (url-copy-file remote tmp t)
;; ;;                                   (rename-file tmp dest t)
;; ;;                                   (neo/log "downloaded payload -> %s" dest)
;; ;;                                   t)
;; ;;                               (error
;; ;;                                (message "[neo/fetch] payload download failed %s: %s"
;; ;;                                         remote (error-message-string err))
;; ;;                                nil))
;; ;;                           (ignore-errors (delete-file tmp))))))
;; ;;               ;; 4) Save the sha sidecar iff we fetched it and (no refresh needed OR refresh succeeded)
;; ;;               (when (and have-remote-sha (or (not need-payload) ok))
;; ;;                 (condition-case err
;; ;;                     (progn
;; ;;                       (copy-file tmp-sha sha-local t t t) ;; overwrite; keep-time; preserve attrs
;; ;;                       (neo/log "saved local sha -> %s" sha-local))
;; ;;                   (error
;; ;;                    (message "[neo/fetch] cannot write local sha %s: %s"
;; ;;                             sha-local (error-message-string err))
;; ;;                    (setq ok nil))))
;; ;;               ok)
;; ;;           (ignore-errors (delete-file tmp-sha)))))
;; ;;      ;; ---------------------------
;; ;;      ;; No remote
;; ;;      ;; ---------------------------
;; ;;      (t
;; ;;       (message "[neo/fetch] no remote for %s" (neo/asset-name a))
;; ;;       nil))))

;; ;; (cl-defmethod neo/fetch ((a neo/asset) &optional force)
;; ;;   "Fetch/build A and cache its NAME.sha256 sidecar.

;; ;; When :remote is a function, it must return the payload as a string. We
;; ;; compute its SHA256 and only rewrite the payload iff FORCE, the file is
;; ;; missing, or the SHA differs. We always refresh NAME.sha256 with the latest
;; ;; hash. When :remote is a URL, behavior is unchanged (download remote
;; ;; NAME.sha256 if available, refresh on FORCE/missing/sha-diff)."
;; ;;   (let* ((dest      (neo/asset-local a))
;; ;;          (sha-local (neo/asset-sha-file-path a))
;; ;;          (remote    (neo/asset-remote a))
;; ;;          (sha-url   (neo/remote-sha-url a)))
;; ;;     (neo/ensure-parent-dir dest)
;; ;;     (neo/ensure-parent-dir sha-local)
;; ;;     (cond
;; ;;      ;; ---------------------------
;; ;;      ;; Builder function branch (returns STRING)
;; ;;      ;; ---------------------------
;; ;;      ((functionp remote)
;; ;;       (message "DOING LAMBDA")
;; ;;       (let* ((payload
;; ;;               (condition-case err
;; ;;                   (funcall remote a)
;; ;;                 (error
;; ;;                  (message "[neo/fetch] builder failed for %s: %s"
;; ;;                           (neo/asset-name a) (error-message-string err))
;; ;;                  nil))))
;; ;;         (unless (stringp payload)
;; ;;           (message "[neo/fetch] builder for %s did not return a string"
;; ;;                    (neo/asset-name a))
;; ;;           (setq payload nil))
;; ;;         (when (null payload) (cl-return-from neo/fetch nil))
;; ;;         ;; Normalize to deterministic on-disk bytes (unibyte, LF)
;; ;;         (let* ((bytes   (encode-coding-string payload 'utf-8-unix)) ; unibyte
;; ;;                (sha-hex (downcase (secure-hash 'sha256 bytes)))
;; ;;                (current (neo/read-sha256-from-file sha-local))
;; ;;                (need    (or force
;; ;;                             (not (file-exists-p dest))
;; ;;                             (not (and current (string-equal current sha-hex))))))
;; ;; 	  (message "SHA %s" sha-hex)
;; ;;           ;; Write/replace payload only if needed
;; ;;           (when need
;; ;;             (condition-case err
;; ;;                 (let ((coding-system-for-write 'no-conversion))
;; ;;                   (with-temp-file dest
;; ;;                     (set-buffer-multibyte nil)
;; ;;                     (insert bytes)))
;; ;;               (error
;; ;;                (message "[neo/fetch] cannot write payload %s: %s"
;; ;;                         dest (error-message-string err))
;; ;;                (cl-return-from neo/fetch nil))))
;; ;;           ;; Always (re)write NAME.sha256 to match what we just built
;; ;;           (condition-case err
;; ;;               (with-temp-file sha-local
;; ;;                 (insert sha-hex) (insert "\n"))
;; ;;             (error
;; ;;              (message "[neo/fetch] cannot write local sha %s: %s"
;; ;;                       sha-local (error-message-string err))
;; ;;              (cl-return-from neo/fetch nil)))
;; ;;           t)))
;; ;;      ;; ---------------------------
;; ;;      ;; URL string branch (unchanged)
;; ;;      ;; ---------------------------
;; ;;      ((stringp remote)
;; ;;       (let ((tmp-sha (make-temp-file "neo-fetch-sha-"))
;; ;;             (have-remote-sha nil)
;; ;;             (need-payload nil)
;; ;;             (ok t))
;; ;;         (unwind-protect
;; ;;             (progn
;; ;;               ;; 1) Try to fetch the remote sha (optional)
;; ;;               (when (stringp sha-url)
;; ;;                 (setq have-remote-sha
;; ;;                       (condition-case err
;; ;;                           (progn
;; ;;                             (url-copy-file sha-url tmp-sha t)
;; ;;                             (neo/log "fetched remote sha: %s" sha-url)
;; ;;                             t)
;; ;;                         (error
;; ;;                          (neo/log "sha unavailable %s: %s"
;; ;;                                   sha-url (error-message-string err))
;; ;;                          nil))))
;; ;;               ;; 2) Decide if we need to refresh payload
;; ;;               (setq need-payload
;; ;;                     (or force
;; ;;                         (not (file-exists-p dest))
;; ;;                         (and have-remote-sha
;; ;;                              (not (and (file-exists-p sha-local)
;; ;;                                        (string-equal (neo/file-bytes tmp-sha)
;; ;;                                                      (neo/file-bytes sha-local)))))))
;; ;;               ;; 3) Refresh payload if needed
;; ;;               (when (and ok need-payload)
;; ;;                 (setq ok
;; ;;                       (let ((tmp (make-temp-file "neo-asset-" nil ".tmp")))
;; ;;                         (unwind-protect
;; ;;                             (condition-case err
;; ;;                                 (progn
;; ;;                                   (url-copy-file remote tmp t)
;; ;;                                   (rename-file tmp dest t)
;; ;;                                   (neo/log "downloaded payload -> %s" dest)
;; ;;                                   t)
;; ;;                               (error
;; ;;                                (message "[neo/fetch] payload download failed %s: %s"
;; ;;                                         remote (error-message-string err))
;; ;;                                nil))
;; ;;                           (ignore-errors (delete-file tmp))))))
;; ;;               ;; 4) Save the sha sidecar iff we fetched it and (no refresh needed OR refresh succeeded)
;; ;;               (when (and have-remote-sha (or (not need-payload) ok))
;; ;;                 (condition-case err
;; ;;                     (progn
;; ;;                       (copy-file tmp-sha sha-local t t t)
;; ;;                       (neo/log "saved local sha -> %s" sha-local))
;; ;;                   (error
;; ;;                    (message "[neo/fetch] cannot write local sha %s: %s"
;; ;;                             sha-local (error-message-string err))
;; ;;                    (setq ok nil))))
;; ;;               ok)
;; ;;           (ignore-errors (delete-file tmp-sha)))))
;; ;;      ;; ---------------------------
;; ;;      ;; No remote
;; ;;      ;; ---------------------------
;; ;;      (t
;; ;;       (message "[neo/fetch] no remote for %s" (neo/asset-name a))
;; ;;       nil))))

;; (cl-defmethod neo/fetch ((a neo/asset) &optional force)
;;   "Fetch/build A. For function/symbol remotes:
;; Unconditionally call the builder to get a STRING, compute its SHA256,
;; and rewrite both payload and NAME.sha256 iff the SHA differs (or FORCE).
;; For URL remotes, unchanged from before."
;;   (let* ((dest      (neo/asset-local a))
;;          (sha-local (neo/asset-sha-file-path a))
;;          (remote    (neo/asset-remote a))
;;          (sha-url   (neo/remote-sha-url a)))
;;     (neo/ensure-parent-dir dest)
;;     (neo/ensure-parent-dir sha-local)
;;     (cond
;;      ;; -------- Builder/symbol: ALWAYS call, write only on change ----------
;;      ((or (symbolp remote) (functionp remote))
;;       (let* ((callable (cond
;;                         ((symbolp remote)
;;                          (unless (fboundp remote)
;;                            (message "[neo/fetch] %S is not fbound" remote)
;;                            (cl-return-from neo/fetch nil))
;;                          (symbol-function remote))
;;                         ((functionp remote) remote)))
;;              (payload (condition-case err
;;                           (funcall callable a)
;;                         (error
;;                          (message "[neo/fetch] builder failed for %s: %s"
;;                                   (neo/asset-name a) (error-message-string err))
;;                          nil))))
;;         (unless (stringp payload)
;;           (message "[neo/fetch] builder for %s must return a string"
;;                    (neo/asset-name a))
;;           (cl-return-from neo/fetch nil))
;;         ;; Deterministic on-disk bytes
;;         (let* ((bytes   (if (multibyte-string-p payload)
;;                             (encode-coding-string payload 'utf-8-unix)
;;                           payload))
;;                (sha-hex (downcase (secure-hash 'sha256 bytes)))
;;                (current (neo/read-sha256-from-file sha-local))
;;                (need    (or force
;;                             (not (file-exists-p dest))
;;                             (not (and current (string-equal current sha-hex))))))
;;           (when need
;;             ;; write payload
;;             (condition-case err
;;                 (let ((coding-system-for-write 'no-conversion))
;;                   (with-temp-file dest
;;                     (set-buffer-multibyte nil)
;;                     (insert bytes)))
;;               (error
;;                (message "[neo/fetch] cannot write %s: %s"
;;                         dest (error-message-string err))
;;                (cl-return-from neo/fetch nil)))
;;             ;; write sha sidecar
;;             (condition-case err
;;                 (with-temp-file sha-local
;;                   (insert sha-hex) (insert "\n"))
;;               (error
;;                (message "[neo/fetch] cannot write sha %s: %s"
;;                         sha-local (error-message-string err))
;;                (cl-return-from neo/fetch nil))))
;;           t)))
;;      ;; ------------------------ URL branch (as before) -----------------------
;;      ((stringp remote)
;;       (let ((tmp-sha (make-temp-file "neo-fetch-sha-"))
;;             (have-remote-sha nil)
;;             (need-payload nil)
;;             (ok t))
;;         (unwind-protect
;;             (progn
;;               ;; 1) try to fetch remote NAME.sha256
;;               (when (stringp sha-url)
;;                 (setq have-remote-sha
;;                       (condition-case err
;;                           (progn (url-copy-file sha-url tmp-sha t) t)
;;                         (error
;;                          (message "[neo/fetch] sha unavailable %s: %s"
;;                                   sha-url (error-message-string err))
;;                          nil))))
;;               ;; 2) decide if payload refresh is needed
;;               (setq need-payload
;;                     (or force
;;                         (not (file-exists-p dest))
;;                         (and have-remote-sha
;;                              (not (and (file-exists-p sha-local)
;;                                        (string-equal (neo/file-bytes tmp-sha)
;;                                                      (neo/file-bytes sha-local)))))))
;;               ;; 3) refresh payload if needed
;;               (when (and ok need-payload)
;;                 (setq ok
;;                       (let ((tmp (make-temp-file "neo-asset-" nil ".tmp")))
;;                         (unwind-protect
;;                             (condition-case err
;;                                 (progn
;;                                   (url-copy-file remote tmp t)
;;                                   (rename-file tmp dest t)
;;                                   t)
;;                               (error
;;                                (message "[neo/fetch] payload download failed %s: %s"
;;                                         remote (error-message-string err))
;;                                nil))
;;                           (ignore-errors (delete-file tmp))))))
;;               ;; 4) cache sha sidecar if we fetched it and (not needed or succeeded)
;;               (when (and have-remote-sha (or (not need-payload) ok))
;;                 (condition-case err
;;                     (copy-file tmp-sha sha-local t t t)
;;                   (error
;;                    (message "[neo/fetch] cannot write sha %s: %s"
;;                             sha-local (error-message-string err))
;;                    (setq ok nil))))
;;               ok)
;;           (ignore-errors (delete-file tmp-sha)))))
;;      ;; ------------------------------ None -----------------------------------
;;      (t
;;       (message "[neo/fetch] no remote for %s" (neo/asset-name a))
;;       nil))))





;; 
;; ;;; High-level API

;; ;; (defun neo/ensure (a &optional force)
;; ;;   "Ensure A is present and valid. Return local path on success, else nil.
;; ;; If :error-on-missing (or default) is non-nil, signal on failure.

;; ;; Important: Even if base verification passes by existence, if the local
;; ;; NAME.sha256 is missing *and* a remote URL is available, we call `neo/fetch`
;; ;; to cache the sidecar."
;; ;;   (let* ((dest (neo/asset-local a))
;; ;;          (errp (neo/error-on-missing-p a))
;; ;;          (sha-local (neo/asset-sha-file-path a))
;; ;;          (have-remote (stringp (neo/asset-remote a))))
;; ;;     (if (and (not force)
;; ;;              (neo/verify a)
;; ;;              ;; Only short-circuit if either we have a local sha already,
;; ;;              ;; or we cannot fetch a remote sha anyway (no URL remote).
;; ;;              (or (file-exists-p sha-local) (not have-remote)))
;; ;;         dest
;; ;;       (let ((ok (neo/fetch a force)))
;; ;;         (cond
;; ;;          ((not ok)
;; ;;           (if errp (error "[neo/asset] fetch failed for %s" dest)
;; ;;             (message "[neo/asset] fetch failed for %s" dest))
;; ;;           nil)
;; ;;          ((neo/verify a) dest)
;; ;;          (t
;; ;;           (if errp (error "[neo/asset] verification failed for %s" dest)
;; ;;             (message "[neo/asset] verification failed for %s" dest))
;; ;;           nil))))))

;; (defun neo/ensure (a &optional force)
;;   "Ensure A is present and valid. Return local path on success, else nil.
;; For function/symbol remotes, always invoke `neo/fetch` so redefinitions
;; take effect immediately; it only rewrites files if the SHA changes (or FORCE)."
;;   (let* ((dest      (neo/asset-local a))
;;          (errp      (neo/error-on-missing-p a))
;;          (remote    (neo/asset-remote a))
;;          (builderp  (or (symbolp remote) (functionp remote)))
;;          ok)
;;     (setq ok
;;           (cond
;;            (force (neo/fetch a t))
;;            (builderp (neo/fetch a nil))                 ;; ← always run builder
;;            ((neo/verify a) t)                           ;; URL & already valid
;;            (t (neo/fetch a nil))))                      ;; URL needs caching
;;     (cond
;;      ((not ok)
;;       (if errp (error "[neo/asset] fetch failed for %s" dest)
;;         (message "[neo/asset] fetch failed for %s" dest))
;;       nil)
;;      ((neo/verify a) dest)
;;      (t
;;       (if errp (error "[neo/asset] verification failed for %s" dest)
;;         (message "[neo/asset] verification failed for %s" dest))
;;       nil))))


;; ;; Back-compat alias:
;; (defalias 'neo/asset-ensure #'neo/ensure)

;; (defun neo/asset-delete (a)
;;   "Delete payload and its NAME.sha256. Return non-nil if anything was deleted."
;;   (let ((deleted nil))
;;     (dolist (f (list (neo/asset-local a) (neo/asset-sha-file-path a)))
;;       (when (file-exists-p f)
;;         (ignore-errors (delete-file f) (setq deleted t))))
;;     deleted))

;; 
;; ;;; Minimal ERT smoke (optional)

;; (eval-when-compile (require 'ert))

;; (ert-deftest neo/asset--paths ()
;;   "Base path computation."
;;   (let* ((tmp (make-temp-file "neo-asset-test-" t))
;;          (neo/asset-cache-dir (file-name-as-directory tmp))
;;          (a (neo/make-asset :name "foo")))
;;     (should (string= (neo/asset-local a)
;;                      (expand-file-name "foo.el" tmp)))
;;     (should (string= (neo/asset-sha-file-path a)
;;                      (expand-file-name "foo.sha256" tmp)))))

;; (ert-deftest neo/ensure-caches-missing-sha-when-remote-exists ()
;;   "Base asset: if local sha missing but remote sha exists, ensure fetches sha."
;;   (let* ((origin (make-temp-file "neo-origin-" t))
;;          (neo/asset-cache-dir (file-name-as-directory (make-temp-file "neo-cache-" t)))
;;          (src (expand-file-name "pkg.el" origin))
;;          (sha (expand-file-name "pkg.sha256" origin)))
;;     ;; origin v1
;;     (with-temp-file src (insert "(message \"v1\")\n"))
;;     (with-temp-file sha (insert (neo/file-sha256 src) "  pkg.el\n"))
;;     (let* ((url (concat "file://" (expand-file-name src)))
;;            (a (neo/make-asset :name "pkg" :remote url)))
;;       (unwind-protect
;;           (progn
;;             ;; First ensure (downloads payload + sha)
;;             (should (neo/ensure a))
;;             (let ((local-sha (neo/asset-sha-file-path a)))
;;               ;; Delete local sha to simulate missing sidecar
;;               (when (file-exists-p local-sha) (delete-file local-sha))
;;               (should (not (file-exists-p local-sha)))
;;               ;; Base verify passes by existence, but ensure should still fetch sha
;;               (should (neo/ensure a))
;;               (should (file-exists-p local-sha))))
;;         (ignore-errors (delete-directory origin t))
;;         (ignore-errors (delete-directory neo/asset-cache-dir t))))))

;; (provide 'neo-asset)
;; ;;; neo-asset.el ends here
