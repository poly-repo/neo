;;; NOTE this first definition wins so that we don't have to remember
;;; to modify the installer and we can simply copy and paste it.
;;; It would be great if elpaca provided that as a downloadable asset
;;; with each release and a summary of changes to go with it, but we
;;; live with what we have and elpaca is great anyhow.
(makunbound 'elpaca-directory)
(defvar elpaca-directory (expand-file-name "elpaca/" no-littering-var-directory))

;;; The following is the installer copied from the elpaca github
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
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


;;; TODO: elisp autocompletion on :custom should be for variables, not functions)

;;; ensure-system-package uses an async buffer (in a side window) which in turns
;;; causes the dashboard to be locked in there, a small window at the bottom of the
;;; frame that cannot even been grown.
;;; TODO: fix the mess for real
;;; for now, I just disable this, as I have everything installed.
(defvar neo/ignore-ensure-system-package t)

(defun neo/filter-package-args (args)
  (let ((ignore-list
         (append
          '(:doc)
          (if neo/ignore-ensure-system-package
              '(:ensure-system-package)
            '()))))
    (mapc (lambda (el) (setq args (map-delete args el))) ignore-list)
    args))

(defvar neo/installed-packages nil)

;;; NOTE: if performance becomes problematic we can move to a hash
;;; table:
;;; (defvar neo/extension-package-map (make-hash-table :test #'equal)
;;;  "Mapping of (USER . EXTENSION) to list of packages used.")
(defvar neo/extension-package-forms nil
  "Alist mapping (USER . EXTENSION) to a list of unexpanded `neo/use-package` forms.")


;;; TODO: add a key 'var' and automatically split between :config and
;;; :custom
;;; NOTE: probably not needed as treating everything as
;;; 'customizeable' should work.
;; (defmacro neo/use-package (name &rest args)
;;   "Augment use-package with Neo specific functionality."
;;   (declare (indent defun))
;;   (setq neo/installed-packages (append neo/installed-packages `(,name)))
;;   (let* ((ensure (if (string= name "emacs") (list :ensure nil) '()))
;; 	 (args (append (neo/filter-package-args args) ensure))
;; 	 (file (or load-file-name
;;                    buffer-file-name
;;                    "unknown")))
;;     (message (format "Expanded in %s" file))
;;     `(use-package
;;       ,name
;;       ;     :elpaca nil
;;       ,@args)))

(defmacro neo/use-package (name &rest args)
  "Augment `use-package` with Neo-specific tracking and filtering.

Stores the partially-expanded use-package form in `neo/extension-package-forms`
indexed by the current user and the base filename of the loading extension."
  (declare (indent defun))
  (let* ((ensure (if (string= name "emacs") (list :ensure nil) '()))
         (args (append (neo/filter-package-args args) ensure))
         (file (or load-file-name buffer-file-name "unknown"))
         (user (user-login-name))
         (extension (file-name-base file))
         (key (cons user extension))
         (real-form `(use-package ,name ,@args))
         (expanded (macroexpand real-form))
         (existing (alist-get key neo/extension-package-forms nil nil #'equal)))
    ;; Update the alist, prepending to the form list
    (setq neo/extension-package-forms
          (cons `(,key . ,(cons expanded existing))
                (assq-delete-all key neo/extension-package-forms)))
    real-form))

(defun neo/replay-extension-packages (&optional user extension)
  "Replay all stored `use-package` expansions.

If USER and EXTENSION are provided, only replays that entry."
  (interactive)
  (dolist (entry neo/extension-package-forms)
    (let ((key (car entry))
          (forms (cdr entry)))
      (when (or (not user)
                (and (equal user (car key))
                     (equal extension (cdr key))))
        (dolist (form forms)
          (eval form))))))


(elpaca
 elpaca-use-package
 ;; Enable :elpaca use-package keyword.
 (elpaca-use-package-mode)
 ;; Assume :elpaca t unless otherwise specified.
 (setq elpaca-use-package-by-default t))

;;; TODO: figure out where duplicate packages get queued
;;; till then is very annoying to see things like:
;;; ⛔ Warning (emacs): Duplicate item queued: ace-window
;;; ⛔ Warning (emacs): Duplicate item queued: magit
;;; ⛔ Warning (emacs): Duplicate item queued: org-roam
;(setq warning-minimum-level :error)

;; Block until current queue processed.
(elpaca-wait)

(provide 'neo-packages)
