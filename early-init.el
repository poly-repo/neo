;;; early-init.el --- Neo Emacs -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Code in this file is executed before init.el, UI initialization or
;; package manager setup.  We do here things that needs to be done this early.

;;; Code:

(defvar neo/paraphenalia-list nil
  "List of UI elements (paraphenalia) to display.

It can take one of the following forms:

  - The symbol `neo/paraphenalia-all`:
    All UI elements are displayed.

  - A list of symbols:
    Only the explicitly listed symbols are displayed. Expected values include:
    `neo/paraphenalia-scrollbar`, `neo/paraphenalia-toolbar`,
    `neo/paraphenalia-menubar`, `neo/paraphenalia-advertisement`,
    `neo/paraphenalia-scratch-message`.

  - nil (default):
    No UI elements are displayed (clean interface).

This variable is used by `neo/paraphenalia` to determine visibility of UI components.")

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

;; Silence intrusive, unactionable warnings from intentional variable aliases
;; and from packages that harmlessly touch lexical-binding — same benign set
;; Doom suppresses.  Real warnings of other types still surface.
(setq warning-suppress-types '((defvaralias) (lexical-binding)))

(add-to-list 'load-path
             (expand-file-name "core" (file-name-directory (or load-file-name buffer-file-name))))
(require 'neo-early-init-utils)

;; Report native-comp async warnings/errors only when debugging (see
;; `neo/debug-p', defined in neo-early-init-utils).  Requires the load-path
;; and require above, hence its placement here rather than at the top.
(setq native-comp-async-report-warnings-errors neo/debug-p)

;; Keep the echo area silent during startup (extension loading, manifest
;; fetches, etc. all call `message'); messages still land in *Messages* for
;; later inspection, they just don't flicker across the minibuffer while
;; booting. Left alone when debugging the init sequence, since neo/debug-p
;; already means "be verbose".
(setq inhibit-message (not neo/debug-p))
(add-hook 'emacs-startup-hook (lambda () (setq inhibit-message nil)))

(eval-and-compile
  (defvar neo/cache-directory (expand-file-name (neo/get-emacs-instance-name) (or (getenv "XDG_CACHE_HOME") "~/.cache")))
  (defvar neo/config-directory (expand-file-name (neo/get-emacs-instance-name) (or (getenv "XDG_CONFIG_HOME") "~/.config")))
  (defvar no-littering-etc-directory neo/config-directory)
  (defvar no-littering-var-directory neo/cache-directory))

(neo/disable-customize-persistence)

(startup-redirect-eln-cache
 (expand-file-name
  (format "eln-cache/%s-emacs-%d.%d/"
          (neo/get-emacs-instance-name)
          emacs-major-version
          emacs-minor-version)
  neo/cache-directory))

(require 'neo-config)

(defvar neo/first-run (let ((pretend-new-user (neo/get-config "pretend-new-user")))
			(or (null pretend-new-user) (string= pretend-new-user "t"))))

(neo/load-config-file "initial-frame-properties.el" t)

;; Guarantee a decent, splittable frame size regardless of what (if anything)
;; was restored above: fresh instances have no saved geometry, and a stale
;; save (e.g. a pop-up/child frame captured at exit, possibly by another
;; instance) can be far too small to split into windows.
(neo/ensure-frame-size-floor)

;; NOTE: deliberately do NOT set `frame-inhibit-implied-resize'.  On this
;; Emacs/GTK3 build the initial frame is intermittently created collapsed to
;; ~200x200px; inhibiting implied resizes only risks keeping that broken frame
;; from being corrected.  The collapse is repaired explicitly above, via
;; `neo/apply-restored-frame-geometry' (retry timers plus a reactive
;; `window-size-change-functions' hook, both defined in
;; `neo-early-init-utils.el' so they run on every boot, not just when the
;; neo:ui extension happens to load).


;; TODO when no existing config is found, we would like to show a
;; pristine Emacs splash screen.  Unfortunately in those conditions
;; elpaca pops up *elpaca-log* and ruins the party. Not sure what is
;; the best way to proceed. Maybe when we bury the log buffer (when
;; there're no errors) we could re-instate the splash screen,
(setq neo/paraphenalia-list 'neo/paraphenalia-all)
(let ((saved-paraphenalia-list (neo/get-config "paraphenalia-config")))
  (when saved-paraphenalia-list
    (setq neo/paraphenalia-list (read saved-paraphenalia-list))))

;; (let ((pretend-new-user (neo/get-config "pretend-new-user")))
;;   (if (or (null pretend-new-user)
;;           (string= pretend-new-user "t"))
;;       (setq neo/paraphenalia-list 'neo/paraphenalia-all)
;;     (unless (neo/load-config-file "early-init-config.el" t)
;;       (setq neo/paraphenalia-list 'neo/paraphenalia-all))))

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


(setq gc-cons-threshold (* 100 1000 1000))
(setq inhibit-compacting-font-caches t)



    
