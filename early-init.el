;;; early-init.el --- Neo Emacs -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Code in this file is executed before init.el, UI initialization or
;; package manager setup.  We do here things that needs to be done this early.

;;; Code:

;  ╭────────────────────────────────────────────────────────────────────────────────────────
;  │ :gear: Early Init/General
;  ╰────────────────────────────────────────────────────────────────────────────────────────

(setq package-enable-at-startup nil)

(setq native-comp-async-report-warnings-errors nil)

;  ╭────────────────────────────────────────────────────────────────────────────────────────
;  │ :gear: Early Init/UI
;  ╰────────────────────────────────────────────────────────────────────────────────────────

;;; TODO: I don't seem to be able to change initial-frame-alist (for the most part we use .Xdefaults, so not a big deal)

(defun neo/default-frame-parameters (params)
  "Add each parameter in PARAMETERS to `default-frame-alist`."
  (dolist (param params)
    (push param initial-frame-alist)
    (push param default-frame-alist)))
  
;; set/inhibit UI config to cure startup flickering
;;; TODO: many of these go in init.el
;;; TODO: cannot get rid of the message on how to get information about the GNU system
(setq
 frame-inhibit-implied-resize t
 frame-resize-pixelwise t
 inhibit-splash-screen t
 inhibit-startup-buffer-menu t
 inhibit-startup-echo-area-message user-login-name
 inhibit-startup-message t
 inhibit-startup-screen t
 menu-bar-mode nil
 ring-bell-function 'ignore
 scroll-bar-mode nil
 tool-bar-mode nil
 use-dialog-box t
 use-file-dialog nil
 use-short-answers t
 )

(set-scroll-bar-mode nil)
(tooltip-mode -1)

(neo/default-frame-parameters '(
                                (width . 800)
                                (height . 900)))

;  ╭────────────────────────────────────────────────────────────────────────────────────────
;  │ :gear: Early Init/Save Areas
;  ╰────────────────────────────────────────────────────────────────────────────────────────

;;; matching setting in init.el
(setq no-littering-etc-directory (expand-file-name ".litter/config" user-emacs-directory))
(setq no-littering-var-directory (expand-file-name ".litter/data" user-emacs-directory))

;; (when (fboundp 'startup-redirect-eln-cache)
;;   (startup-redirect-eln-cache
;;    (convert-standard-filename
;;     (expand-file-name  "var/eln-cache/" user-emacs-directory))))

;; Set eln-cache dir
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache (expand-file-name "eln-cache/" no-littering-var-directory)))


;  ╭────────────────────────────────────────────────────────────────────────────────────────
;  │ :gear: Early Init/Packages
;  ╰────────────────────────────────────────────────────────────────────────────────────────

;; Not necessarily belongs here, but allows us to keep init.el cleaner
;; NOTE: the snippet on the elpaca website uses user-emacs-directory. When replacing this with
;; a new version remember to change that to no-littering-var-directory
;; (see '<==')
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" no-littering-var-directory)) ; <==
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))


