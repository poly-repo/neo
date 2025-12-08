;;; early-init.el --- Neo Emacs -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Code in this file is executed before init.el, UI initialization or
;; package manager setup.  We do here things that needs to be done this early.

;;; Code:

(defvar neo/paraphenalia-list 'neo/paraphenalia-all)

;; we have an option for disabling scrollbars. But even when
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
(require 'neo-early-init-utils)

(eval-and-compile
  (defvar neo/cache-directory (expand-file-name (neo/get-emacs-instance-name) (or (getenv "XDG_CACHE_HOME") "~/.cache")))
  (defvar neo/config-directory (expand-file-name (neo/get-emacs-instance-name) (or (getenv "XDG_CONFIG_HOME") "~/.config"))))
  (defvar no-littering-etc-directory neo/config-directory)
  (defvar no-littering-var-directory neo/cache-directory)

(startup-redirect-eln-cache
 (expand-file-name
  (format "eln-cache/%s-emacs-%d.%d/"
          (neo/get-emacs-instance-name)
          emacs-major-version
          emacs-minor-version)
  neo/cache-directory))


;; TODO when no existing config is found, we would like to show a
;; pristine Emacs splash screen.  Unfortunately in those conditions
;; elpaca pops up *elpaca-log* and ruins the party. Not sure what is
;; the best way to proceed. Maybe when we bury the log buffer (when
;; there're no errors) we could re-instate the splash screen,
(unless (neo/load-config-file "early-init-config.el" t)
  (setq neo/paraphenalia-list 'neo/paraphenalia-all))

(unless (neo/paraphenalia 'neo/paraphenalia-scrollbar)
  (scroll-bar-mode -1))
(unless (neo/paraphenalia 'neo/paraphenalia-toolbar)
  (tool-bar-mode -1))
(unless (neo/paraphenalia 'neo/paraphenalia-menubar)
  (menu-bar-mode -1))
(unless (neo/paraphenalia 'neo/paraphenalia-scratch-message)
  (setq initial-scratch-message nil))
;; Emacs insists on not disabling the advertisement for themselves in the
;; echo area unless (setq inhibit-startup-echo-area-message USERNAME)
;; textually appears in init.el. For some reason they don't look in
;; early-init.el, hence we nuke everything.
;;(if (and (boundp 'neo/nuke-echo-area-message) neo/nuke-echo-area-message)
(unless (neo/paraphenalia 'neo/paraphenalia-advertisement)
  (setq inhibit-startup-screen t)
  (defun startup-echo-area-message () "")
  (defun display-startup-echo-area-message () nil))

(neo/load-config-file "initial-frame-properties.el" t)

(setq gc-cons-threshold (* 100 1000 1000))
(setq inhibit-compacting-font-caches t)



    

