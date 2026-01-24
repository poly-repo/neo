;;; emacs-test-setup.el --- Batch setup for Buttercup tests  -*- lexical-binding: t; -*-

(require 'package)

;; ---- Configuration --------------------------------------------------------

(defconst neo/batch-required-packages
  '(buttercup undercover svg-lib)
  "Packages required to run batch tests.")

(defconst neo/batch-package-dir
  (expand-file-name "emacs-buttercup-packages"
                    (or (getenv "XDG_CACHE_HOME")
                        (expand-file-name "~/.cache")))
  "Directory where batch-installed packages are stored.")

;; ---- Package setup --------------------------------------------------------

(setq package-user-dir neo/batch-package-dir
      package-enable-at-startup nil)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             t)

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (pkg neo/batch-required-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; Optional but useful in batch mode
(setq byte-compile-warnings nil)

(when (require 'undercover nil t)
  (undercover "*.el"
	      "../*.el"
	      ;;	      (:report-format 'simplecov) (:report-file "/home/mav/.local/share/wtrees/mav-209-mvp-workflow-manager/coverage/foo.json" (neo/git-root default-directory)) (:send-report nil)))
	      (:report-format 'simplecov) (:report-file (expand-file-name "coverage/.resultset.json" neo--test-topdir)) (:send-report nil)))

