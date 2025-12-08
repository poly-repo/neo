(defun neo--load-manifest (file)
  "Read and return the top-level form in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (read (current-buffer))))

(defun neo--read-binary-string-safe (file)
  "Return FILE contents as a raw string suitable for embedding in Emacs Lisp."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (buffer-string)))  ;; return raw string, no quoting here

(defun neo--collect-extension-forms (base-dir)
  "Find all manifest.el files two levels deep and embed their emblems."
  (let ((results '()))
    (dolist (dir1 (directory-files base-dir t "^[^.]"))
      (when (file-directory-p dir1)
        (dolist (dir2 (directory-files dir1 t "^[^.]"))
          (when (file-directory-p dir2)
	    (message "Found dir %s" dir2)
	    (message "Result length: %d" (length results))
            (let* ((manifest-file (expand-file-name "manifest.el" dir2))
                   (emblem-file (expand-file-name "emblem64.png" dir2)))
              (when (file-exists-p manifest-file)
		(message "Manifest found")
                (let* ((form (neo--load-manifest manifest-file))
                       (args (if (and (listp form) (eq (car form) 'neo/extension))
                                 (cdr form)
                               form))
                       (form+emblem
                        (if (file-exists-p emblem-file)
                            (append args (list :emblem (neo--read-binary-string-safe emblem-file)))
                          args)))
                  (push form+emblem results))))))))
    (nreverse results)))

(defun neo--compute-sha256-of-file (file)
  "Return SHA256 hex digest of FILE."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defun neo--compute-sha256-of-string (s)
  "Return SHA256 hex digest of S."
  (with-temp-buffer
    (insert s)
    (secure-hash 'sha256 (current-buffer))))

(defun neo--serialize-extensions (forms)
  "Return a string representing FORMS as `neo/extension` calls."
  (with-temp-buffer
    (let ((print-escape-nonascii nil)
          (print-length nil)
          (print-level nil))
      (dolist (form forms)
        (prin1 `(neo/extension ,@form) (current-buffer))
        (terpri (current-buffer))))
    (buffer-string)))

(defun neo--write-extension-file (forms file)
  "Write all FORMS to FILE as a series of `neo/extension` calls."
  (let ((content (neo--serialize-extensions forms)))
    ;; Prevent coding-system munging / newline conversion
    (write-region content nil file nil 0)))

;; maybe should go to utils
(defun neo--project-root ()
  "Return the root directory of the current project, or nil."
  (when-let* ((pr (project-current)))
    (project-root pr)))

;; (defun neo--create-local-extensions-digest (source-dir output-base-dir)
;;   (if (not (file-directory-p source-dir))
;;       (error "Source directory '%s' does not exist, cannot generate extension digest" source-dir)
;;     (let* ((extensions (neo--collect-extension-forms source-dir))
;; 	   (decorated-extensions (neo--serialize-extensions extensions))
;; 	   (sha256 (neo--compute-sha256-of-string decorated-extensions))
;; 	   (output-dir (expand-file-name sha256 output-base-dir))
;; 	   (output-file (expand-file-name "extensions.el" output-dir)))
;;       (message "output-dir %s" output-dir)
;;       (make-directory output-dir t)
;;       (neo--write-extension-file extensions output-file))))

(defun neo--create-local-extensions-digest (source-dir output-base-dir)
  "Generate a digest of extensions from SOURCE-DIR under OUTPUT-BASE-DIR.

This function:
- collects extension forms from SOURCE-DIR,
- serializes them and computes a sha256 digest,
- writes the file \"extensions.el\" into a directory named by the digest
  under OUTPUT-BASE-DIR,
- updates (or creates) a symlink named \"current\" inside OUTPUT-BASE-DIR
  to point at the new digest directory.

If a file named \"current\" already exists and is a symlink, it will be
removed and replaced. If a non-symlink file or directory named \"current\"
exists, the function signals an error to avoid accidental clobbering.

Returns the absolute path of the newly-created output directory."
  (unless (file-directory-p source-dir)
    (error "Source directory '%s' does not exist, cannot generate extension digest"
           source-dir))
  (let* ((extensions (neo--collect-extension-forms source-dir))
         (decorated-extensions (neo--serialize-extensions extensions))
         (sha256 (neo--compute-sha256-of-string decorated-extensions))
         (output-dir (expand-file-name sha256 output-base-dir))
         (output-file (expand-file-name "extensions.el" output-dir))
         (current-link (expand-file-name "current" output-base-dir)))
    (message "neo: output-dir %s" output-dir)
    (make-directory output-dir t)
    (neo--write-extension-file extensions output-file)

    ;; Update 'current' symlink: remove existing symlink if present,
    ;; refuse to replace a non-symlink.
    (when (file-exists-p current-link)
      (if (file-symlink-p current-link)
          (progn
            (message "neo: removing old symlink %s" current-link)
            (delete-file current-link))
        (error "neo: refusing to replace non-symlink %s; remove it manually if you want to update 'current'"
               current-link)))

    ;; Create the new symlink. Use condition-case to give a friendly error.
    (condition-case err
        (progn
          (make-symbolic-link output-dir current-link)
          (message "neo: created symlink %s -> %s" current-link output-dir))
      (error
       (error "neo: failed to create symlink %s -> %s: %s"
              current-link output-dir (error-message-string err))))
    output-dir))

(defun neo--create-github-extensions-digest (source-dir output-base-dir)
  (message "TODO"))

;; TODO we should probably create/update the current symlink
(defun neo/create-local-extension-digest ()
  (interactive)
  ;; TODO check we're inside Omega
  (when-let* ((output-base-dir (format "~/.cache/%s/extensions" (neo/get-emacs-instance-name)))
	      (current-project (neo--project-root))
	      (input-dir (expand-file-name "devex/editors/emacs/extensions/extensions" current-project)))
    (neo--create-local-extensions-digest input-dir output-base-dir)))

(provide 'neo-extensions-digest)



