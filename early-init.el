;;; early-init.el --- Neo Emacs -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Code in this file is executed before init.el, UI initialization or
;; package manager setup.  We do here things that needs to be done this early.

;;; Code:

(setq package-enable-at-startup nil)
(setq native-comp-async-report-warnings-errors nil)

;;; TODO: I don't seem to be able to change initial-frame-alist (for the most part we use .Xdefaults, so not a big deal)
;;; maybe it is lexical scoping at play
;; (defun neo/default-frame-parameters (params)
;;   "Add each parameter in PARAMETERS to `default-frame-alist`."
;;   (dolist (param params)
;;     (push param initial-frame-alist)
;;     (push param default-frame-alist)))
  
;; ;; set/inhibit UI config to cure startup flickering
;; ;;; TODO: many of these go in init.el
;; ;;; TODO: cannot get rid of the message on how to get information about the GNU system
;; (setq
;;  frame-inhibit-implied-resize t
;;  frame-resize-pixelwise t
;;  inhibit-splash-screen t
;;  inhibit-startup-buffer-menu t
;;  inhibit-startup-echo-area-message user-login-name
;;  inhibit-startup-message t
;;  inhibit-startup-screen t
;;  menu-bar-mode nil
;;  ring-bell-function 'ignore
;;  scroll-bar-mode nil
;;  tool-bar-mode nil
;;  use-dialog-box t
;;  use-file-dialog nil
;;  use-short-answers t
;;  )

;; (set-scroll-bar-mode nil)
;; (tooltip-mode -1)

;; (neo/default-frame-parameters '(
;;                                 (width . 800)
;;;                                 (height . 900)))

(add-to-list 'load-path
             (expand-file-name "core" (file-name-directory (or load-file-name buffer-file-name))))
(require 'early-init-utils)


(startup-redirect-eln-cache
 (expand-file-name
  (format "eln-cache/%s-emacs-%d.%d/"
          (neo/get-emacs-instance-name)
          emacs-major-version
          emacs-minor-version)
  (expand-file-name "neo" (or (getenv "XDG_CONFIG_HOME") "~/.cache"))))

(let* ((instance-name (neo/get-emacs-instance-name))
       (early-config (expand-file-name
                      (format "%s-early-init-config.el" instance-name)
                      (expand-file-name "neo" (or (getenv "XDG_CONFIG_HOME") "~/.config")))))
  (message (format "instance name: %s" instance-name))
  (message (format "config: %s [%s]" early-config (file-readable-p early-config)))
  (when (file-readable-p early-config)
    (message (format "Reading %s" early-config))
    (load early-config nil 'nomessage)))

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



;; Load early frame settings if available
(let ((init-file "~/.cache/neo/initial-frame-properties.el"))
  (when (file-readable-p init-file)
    (load-file init-file)))

;; ;; early-init.el
;; (setq default-frame-alist
;;       '((width . 240)
;;         (height . 60)
;;         (font . "Ubuntu Mono-13")
;;         (internal-border-width . 0)
;;         (undecorated . t)))

;; ;; Also pre-define theme-like defaults
;; (set-face-attribute 'default nil :background "#1e1e1e" :foreground "#cccccc")
