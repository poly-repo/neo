;;; neo-elpaca-manager-faces.el --- Per-column faces for Elpaca Manager -*- lexical-binding: t -*-

;;; Commentary:
;; `elpaca-manager''s tabulated-list buffer stamps every column's text with
;; a `tabulated-list-column-name' property (set by both
;; `tabulated-list-init-header' for the header row and
;; `tabulated-list-print-col' for data rows), so column identity can be
;; read directly off any position without recomputing widths/offsets.
;;
;; This file adds:
;; - one shared face for the column title bar, via `face-remap-add-relative'
;;   on the existing `tabulated-list-fake-header' face, scoped to Elpaca
;;   Manager buffers only (other tabulated-list buffers, e.g. `M-x
;;   list-packages', are unaffected)
;; - a distinct face per data column, configurable via
;;   `neo/elpaca-manager-column-faces'
;;
;; The "Package" column is deliberately excluded from the per-column faces:
;; it already carries install-status coloring (blocked/finished/failed/busy)
;; via `elpaca-status-faces', applied lazily by elpaca's own JIT-lock
;; function; remapping it here would silently clobber that signal.
;;
;; Deliberately does NOT `require' elpaca/elpaca-ui/elpaca-manager: those are
;; only on `load-path' once Elpaca's real bootstrap has run, which would make
;; this file unloadable in the `emacs -Q --batch' test sandbox. `add-hook'
;; only needs the hook variable by symbol; it fires later, whenever
;; `elpaca-manager-mode' actually runs.

;;; Code:

(defface neo/elpaca-manager-header
  '((t :weight bold))
  "Face for the Elpaca Manager column title bar.
Composed on top of the shared `tabulated-list-fake-header' face via
`face-remap-add-relative', buffer-locally -- other tabulated-list buffers
\(e.g. `M-x list-packages') keep the stock look."
  :group 'neo-ui)

(defface neo/elpaca-manager-description-column
  '((t :inherit font-lock-doc-face))
  "Face for the Elpaca Manager \"Description\" column."
  :group 'neo-ui)

(defface neo/elpaca-manager-date-column
  '((t :inherit font-lock-constant-face))
  "Face for the Elpaca Manager \"Date\" column."
  :group 'neo-ui)

(defface neo/elpaca-manager-source-column
  '((t :inherit font-lock-type-face))
  "Face for the Elpaca Manager \"Source\" column."
  :group 'neo-ui)

(defcustom neo/elpaca-manager-column-faces
  '(("Description" . neo/elpaca-manager-description-column)
    ("Date"        . neo/elpaca-manager-date-column)
    ("Source"      . neo/elpaca-manager-source-column))
  "Alist mapping Elpaca Manager column names to faces.
Keys must match `tabulated-list-format' labels in `elpaca-manager-mode'
exactly. \"Package\" is intentionally omitted: it already carries
install-status coloring via `elpaca-status-faces', applied by elpaca's
own JIT-lock function; remapping it here would silently clobber that
signal."
  :type '(alist :key-type string :value-type face)
  :group 'neo-ui)

(defun neo/elpaca-manager--apply-column-faces (beg end)
  "Apply `neo/elpaca-manager-column-faces' to data rows between BEG and END.
Registered as a `jit-lock' function (see `jit-lock-register'), so BEG/END
follow that contract. Skips the fake header line/overlay -- the title bar
gets only the shared `neo/elpaca-manager-header' face, applied separately
by `neo/elpaca-manager--setup-faces'."
  (save-excursion
    (goto-char beg) (setq beg (pos-bol))
    (goto-char end) (setq end (pos-eol))
    (with-silent-modifications
      (goto-char beg)
      (while (< (point) end)
        (let ((run-end (or (next-single-property-change
                             (point) 'tabulated-list-column-name nil end)
                            end)))
          (when-let* ((name (get-text-property (point) 'tabulated-list-column-name))
                      (face (assoc-default name neo/elpaca-manager-column-faces))
                      ((not (tabulated-list-header-overlay-p (point)))))
            (add-face-text-property (point) run-end face))
          (goto-char run-end))))
    `(jit-lock-bounds ,beg . ,end)))

(defun neo/elpaca-manager--setup-faces ()
  "Set up Elpaca Manager's title-bar and per-column data faces.
Intended for `elpaca-manager-mode-hook'."
  (face-remap-add-relative 'tabulated-list-fake-header 'neo/elpaca-manager-header)
  (jit-lock-register #'neo/elpaca-manager--apply-column-faces))

(add-hook 'elpaca-manager-mode-hook #'neo/elpaca-manager--setup-faces)

(provide 'neo-elpaca-manager-faces)
;;; neo-elpaca-manager-faces.el ends here
