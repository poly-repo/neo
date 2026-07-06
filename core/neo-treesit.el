;;; -*- lexical-binding: t -*-

;; Central, cross-cutting tree-sitter grammar management. Extensions
;; declare the grammars they need via `:tree-sitter-grammars' in their
;; manifest.el (see the `neo/extension' macro in neo-extensions.el);
;; this file collects those declarations from every installed
;; extension and builds whatever is missing on an idle timer shortly
;; after startup.

(require 'neo-extensions)
(require 'neo-framework)
(require 'treesit)

;; Emacs >=31 asks for confirmation (`treesit-auto-install-grammar'
;; defaults to `ask') before building a grammar the moment some
;; ts-mode needs it, independently of the auto-install below. That
;; race (a ts-mode buffer opening before our idle timer fires) is
;; exactly the "still waits for confirmation" case we're trying to
;; eliminate, so tell Emacs's own fallback to just build it instead of
;; asking. Harmless no-op on older Emacs, which lacks this variable.
(with-no-warnings
  (setq treesit-auto-install-grammar 'always))

(defun neo--treesit-buildinfo-path ()
  "Return the path to neo/buildinfo.md for this Emacs install, or nil.
The file is written by the ansible emacs-install role and lives next
to `invocation-directory' (i.e. peer to bin/ under the install
prefix). Returns nil when the file does not exist, e.g. for a plain
non-ansible-built Emacs."
  (let ((path (expand-file-name
               "neo/buildinfo.md"
               (file-name-directory (directory-file-name invocation-directory)))))
    (when (file-exists-p path) path)))

(defun neo--treesit-runtime-version ()
  "Return the tree-sitter runtime version recorded in neo/buildinfo.md.
Return nil when buildinfo.md does not exist or has no recorded
tree-sitter version."
  (when-let* ((path (neo--treesit-buildinfo-path)))
    (with-temp-buffer
      (insert-file-contents path)
      (when (re-search-forward "^  tree-sitter: \\(\\S-+\\)$" nil t)
        (match-string 1)))))

(defun neo/treesit-grammar-segregation-key ()
  "Return a cache-directory key segregating incompatible grammar ABIs.
Prefers the real tree-sitter runtime version (from neo/buildinfo.md)
over the Emacs major.minor version, since two builds can share an
Emacs version while linking different tree-sitter runtimes."
  (or (neo--treesit-runtime-version)
      (format "emacs-%d.%d" emacs-major-version emacs-minor-version)))

(defconst neo/treesit-grammar-dir
  (neo/cache-file-path
   (format "tree-sitter/%s/" (neo/treesit-grammar-segregation-key)))
  "Directory where NEO installs and loads tree-sitter grammars from.")

(defun neo--collect-tree-sitter-grammars (installed-extensions available-extensions)
  "Merge `:tree-sitter-grammars' declarations from INSTALLED-EXTENSIONS.
INSTALLED-EXTENSIONS is a list of `neo/installation' objects.
AVAILABLE-EXTENSIONS is the slug-string -> `neo/extension' hash table.
Later installations win on duplicate languages; a warning is logged
only when two declarations for the same language actually conflict
(different URL/revision/etc.), not on mere duplicates."
  (let (merged)
    (dolist (installation installed-extensions)
      (let* ((slug-string (neo/extension-slug-to-string
                            (neo/installation-extension-slug installation)))
             (ext (gethash slug-string available-extensions)))
        (when ext
          (dolist (tuple (neo/extension-tree-sitter-grammars ext))
            (when (and (alist-get (car tuple) merged nil nil #'eq)
                       (not (equal (alist-get (car tuple) merged nil nil #'eq) (cdr tuple))))
              (neo/log-warn 'treesit "Conflicting tree-sitter grammar declarations for %s" (car tuple)))
            (setf (alist-get (car tuple) merged nil nil #'eq) (cdr tuple))))))
    merged))

(defun neo/treesit-install-grammars (&optional languages)
  "Install any missing tree-sitter grammar for LANGUAGES.
LANGUAGES defaults to every language in `treesit-language-source-alist'.
Grammars are built into `neo/treesit-grammar-dir'. Kept as an
interactive command for a manual/forced refresh, e.g. after bumping a
pinned revision."
  (interactive)
  (let* ((langs (or languages (mapcar #'car treesit-language-source-alist)))
         (treesit--install-directory neo/treesit-grammar-dir))
    (unless (file-directory-p treesit--install-directory)
      (make-directory treesit--install-directory t))
    (dolist (lang langs)
      (unless (treesit-language-available-p lang)
        (neo/log-info 'treesit "Installing tree-sitter grammar for %s…" lang)
        (treesit-install-language-grammar lang neo/treesit-grammar-dir)))))

(defun neo/treesit-auto-install-grammars ()
  "Collect declared grammars from all installed extensions and build them.
Merges every installed extension's `:tree-sitter-grammars' into
`treesit-language-source-alist' and installs whatever is missing."
  (setq treesit-extra-load-path (list neo/treesit-grammar-dir))
  (let* ((framework (neo--framework-instance))
         (installed (let (l)
                      (maphash (lambda (_k v) (push v l))
                               (neo-framework-installed-extensions framework))
                      l))
         (available (neo-framework-available-extensions framework))
         (merged (neo--collect-tree-sitter-grammars installed available)))
    (dolist (tuple merged)
      (setf (alist-get (car tuple) treesit-language-source-alist nil nil #'eq) (cdr tuple)))
    (neo/treesit-install-grammars)))

;; Defer off the boot path: give every installed extension's manifest
;; and elisp body a chance to run (and register its packages) before
;; we go build grammars, matching the deferred-idle-timer convention
;; used elsewhere in NEO (e.g. neo-projects.el's perspective restore).
(add-hook 'neo/after-framework-bootstrap-hook
          (lambda () (run-with-idle-timer 1 nil #'neo/treesit-auto-install-grammars)))

(provide 'neo-treesit)
