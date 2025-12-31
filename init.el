;;; neo --- My Emacs configuration, in its N-th reincarnation
;;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Where the magic begins ✨ [well, that would be early-init.el 🤪]
;;; It is what I like to use. Most likely is not what you'd like to
;;; use and vscode might serve you better.

;; Abandon all hope, ye who enter uncustomized.

;;; Code:

;;; NOTE: we keep the eln cache out of here as we need to set it very
;;; early in early-init.el and I didn't want to duplicate the
;;; definitions of the no-littering directories. Natively compiled
;;; modules are in
;;; XDG_CONFIG_HOME/.cache/neo/eln-cache/emacs-neo-devel-emacs-31.0
;;(defvar no-littering-etc-directory (expand-file-name ".litter/config" user-emacs-directory))
;;(defvar no-littering-var-directory (expand-file-name ".litter/data" user-emacs-directory))

;; (defvar neo/cache-directory)
;; (defvar neo/config-directory)

;; trivial comment for CI testing. Remove next time this file is touched. Or not.

(defvar neo/minimum-emacs-version "30.1")

(unless (version<= neo/minimum-emacs-version emacs-version)
  ;; TODO offer to build a newer version
  ;; TODO check OS, we can only build on debian
  (message "Emacs version is not good enough"))

;; Ensure the directory exists
(unless (file-directory-p neo/cache-directory)
  (make-directory neo/cache-directory t))

;; Relocate auto-save-list to cache
(setq auto-save-list-file-prefix
      (expand-file-name "auto-save-list/.saves-" neo/cache-directory))

;; Create the auto-save-list directory upfront to avoid any race
(let ((asl-dir (file-name-directory auto-save-list-file-prefix)))
  (unless (file-directory-p asl-dir)
    (make-directory asl-dir t)))

(dolist (subdir '("core"))
  (add-to-list 'load-path (expand-file-name subdir user-emacs-directory)))

;; TODO not sure if needed. I think I had problems with lexical binding
;(require 'neo-early-init-utils)
(require 'neo)

