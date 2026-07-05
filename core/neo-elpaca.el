;;; neo-elpaca.el --- Elpaca bootstrap with a no-littering redirect -*- lexical-binding: t -*-
;;
;; ---------------------------------------------------------------------------
;; Elpaca bootstrap (installer version 0.12)
;; ---------------------------------------------------------------------------
;; The installer block further down (between the BEGIN/END markers) is copied
;; VERBATIM from Elpaca's official installer so we can refresh it by plain
;; copy-paste when Elpaca ships a new version.  Our only desired change vs
;; upstream is to place Elpaca's tree under `no-littering-var-directory'
;; (~/.cache/<instance>) instead of `user-emacs-directory', to keep the config
;; dir clean.
;;
;; We do that WITHOUT editing the pasted block, by pre-binding `elpaca-directory'
;; here, before the installer runs.  Two `defvar' guarantees make this work:
;;   1. `defvar' does NOT change a variable that already has a value; and
;;   2. in that case it does not even EVALUATE its value form.
;; So once we bind `elpaca-directory' below, the installer's own
;; `(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))'
;; is a pure no-op, and `elpaca-builds-directory'/`elpaca-sources-directory'
;; (also verbatim) derive from OUR value.
;;
;; `makunbound' first makes the redirect win even if something already bound
;; `elpaca-directory' (Elpaca preloaded, or this file re-evaluated).
;;
;; INVARIANT: this pre-binding MUST run BEFORE the installer block.  If a future
;; paste lands above it, or it is reordered, the redirect silently fails and
;; Elpaca litters `user-emacs-directory' with no error — the `cl-assert' right
;; after the installer guards against exactly that.
;;
;; DEVIATIONS from pure-verbatim are kept OUTSIDE the marked region, after it
;; (an `advice-add' for enqueue de-duplication and a `-90' depth on the
;; `after-init-hook').  Requires `no-littering-var-directory' (from early-init).
;; ---------------------------------------------------------------------------

(require 'cl-lib)                       ; for `cl-assert' and `cl-loop' below

(makunbound 'elpaca-directory)
(defvar elpaca-directory (expand-file-name "elpaca/" no-littering-var-directory))

(defun neo/elpaca-hide-successful-log ()
  "Hide Elpaca log buffer if queues processed successfully."
  ;; TODO the one second delay here cannot be right or good. But for now I have nothing better
  (run-at-time 1 nil
	       (lambda ()
		 (let ((incomplete
			(cl-loop for q in elpaca--queues
				 unless (cl-loop for (_ . e) in (elpaca-q<-elpacas q)
						 always (memq (elpaca<-status e) '(finished failed)))
				 when (elpaca-q<-elpacas q) return q)))
		   (unless incomplete
		     (when-let ((log (bound-and-true-p elpaca-log-buffer))
				(window (get-buffer-window log t)))
		       (with-selected-window window (quit-window 'kill window))))))))

(add-hook 'elpaca-after-init-hook #'neo/elpaca-hide-successful-log)
(add-hook 'elpaca-post-queue-hook #'neo/elpaca-hide-successful-log)

(defun neo--elpaca-enqueue-deduplicate (orig order &optional queue)
  "Return an existing Elpaca entry when ORDER is already queued."
  (let ((id (condition-case nil
                (elpaca--first order)
              (error nil))))
    (if (and id
             (not after-init-time)
             (elpaca-get id))
        (elpaca-get id)
      (funcall orig order queue))))


;; TODO lock files are still in flux, but if you never want to update they work.
; (defvar elpaca-lock-file "~/elpacas.el")

;; (add-hook 'elpaca-post-queue-hook #'+elpaca-hide-successful-log)

;;; >>> BEGIN elpaca installer (v0.12) — VERBATIM, do not edit inside >>>
;;; Refresh by pasting a newer upstream installer here.  `elpaca-directory' is
;;; redirected by the pre-binding at the top of this file (see header); the
;;; `(defvar elpaca-directory …)' just below is therefore intentionally a no-op.
(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca-activate)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
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
;;; <<< END elpaca installer (v0.12) <<<

;; Fail loudly if the redirect at the top did not take (see header INVARIANT):
;; a partial or reordered paste would otherwise silently litter
;; `user-emacs-directory'.
(cl-assert (string-prefix-p (expand-file-name no-littering-var-directory)
                            (expand-file-name elpaca-directory))
           t "NEO: elpaca-directory redirect failed — the no-littering \
pre-binding must precede the installer block (got %s)" elpaca-directory)

;; --- NEO additions (NOT part of the verbatim installer) ---
(advice-add 'elpaca--enqueue :around #'neo--elpaca-enqueue-deduplicate)
(add-hook 'after-init-hook #'elpaca-process-queues -90)
(elpaca `(,@elpaca-order))

(setopt elpaca-ui-row-limit most-positive-fixnum)

(elpaca
 elpaca-use-package
 ;; Enable :ensure support backed by Elpaca recipes.
 (elpaca-use-package-mode)
 ;; `use-package-always-ensure' must stay boolean because use-package wraps
 ;; defaults again before Elpaca sees them. `neo/use-package' adds the explicit
 ;; `:ensure (:wait t)' form for Neo-managed declarations that need waiting.
 (setq use-package-always-ensure t))

; compat is on GNU ELPA only; install via git to avoid needing the GNU ELPA cache
(elpaca (compat :host github :repo "emacs-compat/compat"))
(elpaca dash)				; used in neo-list-utils

;; Block until current queue processed.
(elpaca-wait)

(require 'neo-application)
(neo/application "Elpaca Manager"
  :setup (elpaca-manager)
  :bind "e")

(provide 'neo-elpaca)
