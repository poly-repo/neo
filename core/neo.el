;;; -*- lexical-binding: t -*-

;; `neo--mode' and the `neo/debug-p' switch are defined in
;; neo-early-init-utils (loaded from early-init.el).  Require it here so this
;; file also works when loaded directly (tests, byte-compilation).
(require 'neo-early-init-utils)

;; Keep startup quiet by default; enable backtraces and a large message log
;; only when debugging (emacs --debug-init, or NEO_MODE=debug).
(when neo/debug-p
  (setq debug-on-error t)
  (setq message-log-max 100000))

;;;###autoload
(defvar neo/use-extensions  t
  "When t we use neo extension mechanism. nil make immediate use of use-package")

(defun neo--require (feature)
  "A wrapper around `require` for FEATURE."
  (require feature))

(neo--require 'neo-utils)
(neo--require 'neo-window)
(neo--require 'neo-custom)			; customization groups
(neo--require 'neo-config)			; configuration db
(neo--require 'neo-application)			; needed in neo-packages
(neo--require 'neo-packages)			; package management
(neo--require 'neo-logging)
(neo--require 'neo-framework)			; neo infra and core extension management
(neo--require 'neo-processes)
(neo--require 'neo-fonts)

(require 'server)

(unless (server-running-p)
  (server-start))

(neo/set-emacs-source-directories)

(setq load-prefer-newer t)
;(use-package project :ensure t)

(neo/maybe-fetch-extensions)

(setq neo--framework (neo--framework-instance))
(neo/bootstrap neo--framework)

(provide 'neo)
