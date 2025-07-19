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
;;; TODO check if that .cache subdir is ok for XDG naming.
(defvar no-littering-etc-directory (expand-file-name ".litter/config" user-emacs-directory))
(defvar no-littering-var-directory (expand-file-name ".litter/data" user-emacs-directory))

(dolist (subdir '("core"))
  (add-to-list 'load-path (expand-file-name subdir user-emacs-directory)))

(require 'early-init-utils)
(require 'neo)

;;; TODO temp only, but is useful to have access to all NEO .el file
;;; from the dev Emacs we launch for testing things
(defun neo/visit-all-elisp-files (&optional dir)
  "Recursively visit all .el files under DIR (default: `user-emacs-directory`)."
  (let ((root (file-name-as-directory (or dir user-emacs-directory))))
    (dolist (file (directory-files-recursively root "\\.el\\'"))
      (find-file-noselect file))))
(setq vc-follow-symlinks t)
(setq enable-local-variables :all)
(neo/visit-all-elisp-files)
