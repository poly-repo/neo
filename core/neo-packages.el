;; -*- lexical-binding: t -*-
;(require 'neo-extensions)
;(require 'neo-extensions-digest)
(require 'neo-elpaca)
(require 'neo-use-package)

;; TODO: resolve the mess w/ :ensure-system-packages
;; TODO: elisp autocompletion on :custom should be for variables, not functions)

;;; ensure-system-package uses an async buffer (in a side window) which in turns
;;; causes the dashboard to be locked in there, a small window at the bottom of the
;;; frame that cannot even been grown.
;;; TODO: fix the mess for real
;;; for now, I just disable this, as I have everything installed.
(defcustom neo/ignore-ensure-system-package t
  "If non-nil, disables the use of `ensure-system-package` in `neo/use-package`.

This is useful when system dependencies are already satisfied or managed externally.
But it was introduced because elpaca had a problem with it:
  ensure-system-package uses an async buffer (in a side window) which in turns
  causes the dashboard to be locked in there, a small window at the bottom of the
  frame that cannot even been grown.
"
  :type 'boolean
  :group 'neo-packages)

;(defvar neo--installed-packages nil) ; TODO is this used

;;; NOTE: if performance becomes problematic we can move to a hash
;;; table:
;;; (defvar neo/extension-package-map (make-hash-table :test #'equal)
;;;  "Mapping of (USER . EXTENSION) to list of packages used.")
(defvar neo--enabled-packages nil
  "Alist mapping (USER . EXTENSION) to a list of unexpanded `neo/use-package` forms.")

(setq package-install-upgrade-built-in t)

(setopt package-archives
        '(("gnu" . "https://elpa.gnu.org/packages/")
          ("nongnu" . "https://elpa.nongnu.org/nongnu/")
          ("melpa" . "https://melpa.org/packages/")))


(defun neo/replay-extension-packages (&optional slug)
  "Replay all stored `use-package` expansions.

If SLUG is provided, a `neo/extension-slug` object, only replays that entry."
  (interactive)
  (let ((user (when slug (neo/extension-slug-publisher slug)))
        (extension (when slug (neo/extension-slug-name slug))))
    (dolist (entry neo--enabled-packages)
      (let ((key (car entry))
            (forms (cdr entry)))
        (when (or (not slug)
                  (and (equal user (car key))
                       (equal extension (cdr key))))
          (dolist (form forms)
            (eval form)))))))

;;; TODO: figure out where duplicate packages get queued
;;; till then is very annoying to see things like:
;;; ⛔ Warning (emacs): Duplicate item queued: ace-window
;;; ⛔ Warning (emacs): Duplicate item queued: magit
;;; ⛔ Warning (emacs): Duplicate item queued: org-roam
;(setq warning-minimum-level :error)

;; probably need to ensure it is run early
(use-package no-littering
  :ensure t
  :init
  ;; We define these in early-init.el so everything can be kept out of the way
  ;; in particular elpaca and eln-cache
  ;(setq no-littering-etc-directory (expand-file-name ".litter/config" user-emacs-directory))
  ;(setq no-littering-var-directory (expand-file-name ".litter/data" user-emacs-directory))
  (setq custom-file
        (expand-file-name "custom.el" no-littering-var-directory))
  :config
  (no-littering-theme-backups)
  (setq auto-save-file-name-transforms
        `((".*"
           ,(no-littering-expand-var-file-name "auto-save/")
           t))))


(provide 'neo-packages)
