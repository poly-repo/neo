;;; neo-wizard-client.el --- Control a sandbox Emacs via websocket -*- lexical-binding: t -*-
;;; Commentary:
;; - Starts a sandbox Emacs process loading `neo-wizard-sandbox.el`.
;; - Connects via websocket to send s-expression commands.
;; - Allows previewing theme, font, geometry, and shutdown.
;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'websocket)


(defgroup neo-wizard nil
  "Sandbox Emacs control client."
  :group 'convenience)

(defcustom neo-wizard-sandbox-init-directory
  (expand-file-name "~/neo/preview")
  "Directory containing the sandbox Emacs init.el."
  :type 'string
  :group 'neo-wizard)

(defconst neo-wizard-port 9200
  "Fixed websocket port for the sandbox Emacs.")

(defvar neo-wizard--proc nil
  "Process object of the sandbox Emacs.")

(defvar neo-wizard--ws nil
  "Websocket client to the sandbox.")

;;; --- Process start ---------------------------------------------------------
(defun neo-wizard-start-sandbox ()
  "Start the sandbox Emacs process and connect websocket."
  (interactive)
  (cl-block nil
    (when (and neo-wizard--proc (process-live-p neo-wizard--proc))
      (message "[neo-client] Sandbox already running")
      (cl-return-from nil))  ;; exits cl-block
    ;; Command to start GUI Emacs with fixed init directory
    (let ((cmd (list "emacs" "--init-directory" neo-wizard-sandbox-init-directory)))
      (setq neo-wizard--proc
            (apply #'start-process "neo-wizard-sandbox" nil (car cmd) (cdr cmd)))
      (message "[neo-client] started sandbox pid=%d" (process-id neo-wizard--proc))
      ;; Give time for the server to start
      (sleep-for 0.6)
      ;; Connect websocket
      (neo-wizard--connect-ws neo-wizard-port)
      (message "[neo-client] sandbox ready on port %d" neo-wizard-port))))


;;; --- Websocket helpers ----------------------------------------------------
(defun neo-wizard--connect-ws (port)
  "Connect to sandbox websocket on PORT."
  (setq neo-wizard--ws
        (websocket-open
         (format "ws://127.0.0.1:%d/" port)
         :on-message #'neo-wizard--on-message
         :on-close #'neo-wizard--on-close))
  (unless (websocket-openp neo-wizard--ws)
    (error "[neo-client] Failed to open websocket to port %d" port)))

(defun neo-wizard--on-message (_ws frame)
  "Handle incoming messages from sandbox WS FRAME."
  (let ((payload (websocket-frame-payload frame)))
    (message "[neo-client] RECV: %s" payload)))

(defun neo-wizard--on-close (_ws)
  "Handle websocket closure."
  (message "[neo-client] sandbox websocket closed"))

(defun neo-wizard--send-sexp (sexp)
  "Send SEXP to sandbox via websocket."
  (unless (and neo-wizard--ws (websocket-openp neo-wizard--ws))
    (error "[neo-client] websocket not connected"))
  (websocket-send-text neo-wizard--ws (prin1-to-string sexp)))

;;; --- Commands -------------------------------------------------------------
(defun neo-wizard-preview-theme (theme-name)
  "Ask sandbox to load THEME-NAME."
  (interactive
   (list (completing-read "Theme: " (mapcar #'symbol-name (custom-available-themes)) nil t)))
  (neo-wizard--send-sexp `(command load-theme ,theme-name)))



(defun neo-wizard-preview-font (font-name size)
  "Ask sandbox to set FONT-NAME and optional SIZE."
  (interactive
   (let ((family (completing-read "Font: " (font-family-list) nil t))
         (sz (read-number "Point size: " 14)))
     (list family sz)))
  (neo-wizard--send-sexp (list 'command 'set-font font-name size)))

(defun neo-wizard-set-geometry (cols rows)
  "Ask sandbox to set frame geometry to COLSxROWS."
  (interactive
   (list (read-number "Cols: " 80)
         (read-number "Rows: " 30)))
  (neo-wizard--send-sexp `(command set-geometry ,cols ,rows)))

(defun neo-wizard-shutdown ()
  "Request sandbox to shutdown and cleanup."
  (interactive)
  (when (and neo-wizard--ws (websocket-openp neo-wizard--ws))
    (neo-wizard--send-sexp '(command shutdown)))
  (when (and neo-wizard--proc (process-live-p neo-wizard--proc))
    (kill-process neo-wizard--proc))
  (setq neo-wizard--proc nil)
  (setq neo-wizard--ws nil)
  (message "[neo-client] sandbox stopped"))
