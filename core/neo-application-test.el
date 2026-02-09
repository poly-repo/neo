;;; neo-application-test.el --- Test for Neo applications -*- lexical-binding: t -*-

(require 'buttercup)
(require 'cl-lib)

;; Mock perspective before requiring neo-application
(defvar persp-mode nil)
(defvar persp-names '("Default"))
(defvar persp-curr-name "Default")
(defvar persp-buffers-list nil) ; buffers in current perspective

(defun persp-mode (arg)
  (setq persp-mode (if (> arg 0) t nil)))

(defun persp-switch (name)
  (setq persp-curr-name name)
  ;; Ensure scratch buffer exists for new perspective
  (let ((scratch-name (format "*scratch* (App:%s)" (string-remove-prefix "App:" name))))
    (get-buffer-create scratch-name)))

(defun persp-current-name () persp-curr-name)

(defun persp-names () persp-names)

(defun persp-remove-buffer (buf)
  (setq persp-buffers-list (remove buf persp-buffers-list)))

(defun persp-curr ()
  (list :name persp-curr-name :buffers persp-buffers-list))

(defun persp-buffers (persp)
  persp-buffers-list)

(provide 'perspective)

(require 'neo-application)

(describe "neo-application"
    (it "cleans up scratch and messages windows after setup"
      (let ((test-app-name "TestApp")
            (scratch-buf-name "*scratch* (App:TestApp)")
            (app-buf-name "*TestApp*"))
        
        (neo/application "TestApp"
          :setup (switch-to-buffer (get-buffer-create "*TestApp*")))
        
        ;; Setup mock state
        (setq persp-curr-name "Default")
        (setq persp-buffers-list nil)
        
        ;; Create windows to simulate the state
        (delete-other-windows)
        (switch-to-buffer (get-buffer-create scratch-buf-name))
        (split-window-right)
        (other-window 1)
        (switch-to-buffer (get-buffer-create "*Messages*"))
        (split-window-right)
        (other-window 1)
        (switch-to-buffer (get-buffer-create "*Warnings*"))
        
        ;; We have 3 windows: scratch, messages, warnings.
        ;; Calling the app function:
        ;; 1. persp-switch -> updates mock state.
        ;; 2. setup -> switch-to-buffer "*TestApp*" in CURRENT window (the warnings one).
        ;; 3. cleanup -> should see the OTHER windows have scratch and messages, and delete them.
        ;;    The current window has *TestApp*, so it stays.
        
        (neo/app-testapp)
        
        (expect (get-buffer-window app-buf-name) :to-be-truthy)
        (expect (get-buffer-window scratch-buf-name) :not :to-be-truthy)
        (expect (get-buffer-window "*Messages*") :not :to-be-truthy)
        (expect (get-buffer-window "*Warnings*") :not :to-be-truthy)
        (expect (length (window-list)) :to-be 1)
        ))

  (it "allows switching between applications"
    (neo/application "App1" :setup (ignore))
    (neo/application "App2" :setup (ignore))

    ;; Start in Default
    (setq persp-curr-name "Default")
    (setq neo--last-user-perspective nil)

    ;; Switch to App1
    (neo/app-app1)
    (expect persp-curr-name :to-equal "App:App1")
    (expect neo--last-user-perspective :to-equal "Default")

    ;; Switch to App2 directly from App1
    (neo/app-app2)
    (expect persp-curr-name :to-equal "App:App2")
    ;; Should still point to Default, not App:App1
    (expect neo--last-user-perspective :to-equal "Default")
    ))