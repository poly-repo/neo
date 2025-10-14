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
(defvar no-littering-etc-directory (expand-file-name ".litter/config" user-emacs-directory))
(defvar no-littering-var-directory (expand-file-name ".litter/data" user-emacs-directory))

(defvar neo/cache-directory)
(defvar neo/config-directory)

(dolist (subdir '("core"))
  (add-to-list 'load-path (expand-file-name subdir user-emacs-directory)))

(require 'early-init-utils)
(require 'neo)


