;;; early-init.el --- Neo Emacs -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Code in this file is executed before init.el, UI initialization or
;; package manager setup.  We do here things that needs to be done this early.

;;; Code:

;; we'll have an option for disabling scrollbars. But even when
;; enabled, there's no reason to have them in the minibuffer.
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (set-window-scroll-bars
             (minibuffer-window frame) 0 nil 0 nil t)
            (set-window-fringes
             (minibuffer-window frame) 0 0 nil t)))

;; prevent package.el from doing things, we'll use elpaca and use-package

(setq package-enable-at-startup nil)
(setq package-activated-list nil)

(setq native-comp-async-report-warnings-errors nil)

(add-to-list 'load-path
             (expand-file-name "core" (file-name-directory (or load-file-name buffer-file-name))))
(require 'early-init-utils)

;; (setq neo/cache-directory (expand-file-name (neo/get-emacs-instance-name) (or (getenv "XDG_CONFIG_HOME") "~/.cache")))
;; (setq neo/config-directory (expand-file-name (neo/get-emacs-instance-name) (or (getenv "XDG_CONFIG_HOME") "~/.config")))

(eval-and-compile
  (defvar no-littering-etc-directory
    (expand-file-name ".litter/config" user-emacs-directory))
  (defvar no-littering-var-directory
    (expand-file-name ".litter/data" user-emacs-directory))
  (defvar neo/cache-directory (expand-file-name (neo/get-emacs-instance-name) (or (getenv "XDG_CONFIG_HOME") "~/.cache")))
  (defvar neo/config-directory (expand-file-name (neo/get-emacs-instance-name) (or (getenv "XDG_CONFIG_HOME") "~/.config"))))


(startup-redirect-eln-cache
 (expand-file-name
  (format "eln-cache/%s-emacs-%d.%d/"
          (neo/get-emacs-instance-name)
          emacs-major-version
          emacs-minor-version)
  neo/cache-directory))

(let* ((instance-name (neo/get-emacs-instance-name))
       (early-config (expand-file-name
                      (format "%s-early-init-config.el" instance-name)
		      neo/config-directory)))
  (when (not (file-readable-p early-config))
    (message "EARLY CONFIG NOT FOUND IN %s" early-config))
  (when (file-readable-p early-config)
    (message "LOADING EARLY CONFIG FROM %s" early-config)
    (load early-config nil 'nomessage)))

;(scroll-bar-mode -1)

(setq gc-cons-threshold (* 100 1000 1000))
(setq inhibit-compacting-font-caches t)

;;; Emacs insists on disabling the advertisement for themselves in the
;;; echo area unless (setq inhibit-startup-echo-area-message USERNAME)
;;; textually appears in init.el. For some reason they don't look in
;;; early-init.el, hence we nuke everything.
(if neo/nuke-echo-area-message
    (progn
      (defun startup-echo-area-message () "")
      (defun display-startup-echo-area-message () nil)))

;; Set a default font early to prevent frame resizing.
;; This should match the font loaded by your theme.
;; TODO should go in neo-devel-early-init-config
(set-face-attribute 'default nil :family "Noto Mono" :height 120)

;; Load early frame settings if available
(let ((init-file (expand-file-name "initial-frame-properties.el" neo/cache-directory)))
  (when (file-readable-p init-file)
    (load-file init-file)))

