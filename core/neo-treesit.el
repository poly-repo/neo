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

(defun neo--collect-tree-sitter-mode-preferences (installed-extensions available-extensions)
  "Merge `:tree-sitter-modes' declarations from INSTALLED-EXTENSIONS.
INSTALLED-EXTENSIONS is a list of `neo/installation' objects.
AVAILABLE-EXTENSIONS is the slug-string -> `neo/extension' hash table.
Later installations win on duplicate classic modes; a warning is
logged only when two declarations for the same classic mode actually
conflict (different target ts-mode), not on mere duplicates. Unlike
`neo--collect-tree-sitter-grammars', conflicts are keyed on
CLASSIC-MODE rather than LANG, since that's what
`major-mode-remap-alist' itself is keyed by — two extensions targeting
the same LANG with different CLASSIC-MODE packages is not a conflict.
Returns an alist of CLASSIC-MODE -> (LANG . TS-MODE)."
  (let (merged)
    (dolist (installation installed-extensions)
      (let* ((slug-string (neo/extension-slug-to-string
                            (neo/installation-extension-slug installation)))
             (ext (gethash slug-string available-extensions)))
        (when ext
          (dolist (tuple (neo/extension-tree-sitter-modes ext))
            (let* ((lang (nth 0 tuple))
                   (classic-mode (nth 1 tuple))
                   (ts-mode (nth 2 tuple))
                   (existing (alist-get classic-mode merged nil nil #'eq)))
              (when (and existing (not (eq (cdr existing) ts-mode)))
                (neo/log-warn 'treesit "Conflicting tree-sitter mode declarations for %s" classic-mode))
              (setf (alist-get classic-mode merged nil nil #'eq) (cons lang ts-mode)))))))
    merged))

(defcustom neo/treesit-disable-mode-preferences nil
  "When non-nil, disable all NEO tree-sitter major-mode preferences.
Global emergency kill-switch for `neo/treesit-apply-mode-preferences',
covering every extension's `:tree-sitter-modes' declarations at once,
in case a grammar/query version drift regression appears before a
maintainer re-pins the affected grammar."
  :type 'boolean
  :group 'neo)

(defun neo/treesit-apply-mode-preferences ()
  "Register or clear `major-mode-remap-alist' entries per `:tree-sitter-modes'.
Collects every installed extension's declared (LANG CLASSIC-MODE
TS-MODE) preferences and, for each, remaps CLASSIC-MODE to TS-MODE
once LANG's grammar is ready and TS-MODE is loadable; otherwise clears
any stale remap for CLASSIC-MODE. Kept as an interactive command so it
can be re-run mid-session after installing a grammar, paralleling
`neo/treesit-install-grammars'. No-op when
`neo/treesit-disable-mode-preferences' is non-nil."
  (interactive)
  (unless neo/treesit-disable-mode-preferences
    (let* ((framework (neo--framework-instance))
           (installed (let (l)
                        (maphash (lambda (_k v) (push v l))
                                 (neo-framework-installed-extensions framework))
                        l))
           (available (neo-framework-available-extensions framework))
           (merged (neo--collect-tree-sitter-mode-preferences installed available)))
      (dolist (entry merged)
        (let* ((classic-mode (car entry))
               (lang (cadr entry))
               (ts-mode (cddr entry))
               (ready (and (treesit-ready-p lang t) (fboundp ts-mode)))
               (existing (alist-get classic-mode major-mode-remap-alist)))
          (if ready
              (progn
                (when (and existing (not (eq existing ts-mode)))
                  (neo/log-info 'treesit "Overriding existing major-mode-remap-alist entry for %s (%s -> %s)"
                                classic-mode existing ts-mode))
                (setf (alist-get classic-mode major-mode-remap-alist) ts-mode))
            (setq major-mode-remap-alist
                  (assq-delete-all classic-mode major-mode-remap-alist))))))))

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
    (neo/treesit-install-grammars)
    (neo/treesit-apply-mode-preferences)))

;; Defer off the boot path: give every installed extension's manifest
;; and elisp body a chance to run (and register its packages) before
;; we go build grammars, matching the deferred-idle-timer convention
;; used elsewhere in NEO (e.g. neo-projects.el's perspective restore).
(add-hook 'neo/after-framework-bootstrap-hook
          (lambda () (run-with-idle-timer 1 nil #'neo/treesit-auto-install-grammars)))

(provide 'neo-treesit)
