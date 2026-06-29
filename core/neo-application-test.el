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

  (it "stores registered applications and exposes them"
    (neo/application "Stored" :setup (ignore) :bind "s")
    (let ((app (gethash "Stored" neo--applications)))
      (expect app :to-be-truthy)
      (expect (neo/application-name app) :to-equal "Stored")
      (expect (neo/application-binding app) :to-equal "s")
      (expect (neo/application-command app) :to-equal 'neo/app-stored))
    (expect (neo/application-names) :to-contain "Stored")
    (expect (cl-every #'neo/application-p (neo/applications)) :to-be-truthy))

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
    (expect neo--last-user-perspective :to-equal "Default"))

  (it "registers default quit keys and honors per-app overrides"
    (neo/application "QuitDefault" :setup (ignore))
    (neo/application "QuitNone" :setup (ignore) :quit-keys nil)
    (neo/application "QuitCustom" :setup (ignore) :quit-keys ("Q" "x"))
    (expect (neo/application-quit-keys (gethash "QuitDefault" neo--applications))
            :to-equal '("q"))
    (expect (neo/application-quit-keys (gethash "QuitNone" neo--applications))
            :to-equal nil)
    (expect (neo/application-quit-keys (gethash "QuitCustom" neo--applications))
            :to-equal '("Q" "x")))

  (it "installs a quit key that leaves the application from its buffer"
    (neo/application "QuitBuf"
      :setup (switch-to-buffer (get-buffer-create "*QuitBuf*")))
    (setq persp-curr-name "Default")
    (setq neo--last-user-perspective nil)
    (delete-other-windows)
    (neo/app-quitbuf)
    (with-current-buffer "*QuitBuf*"
      (let* ((entry (assq 'neo-application-mode minor-mode-overriding-map-alist))
             (map (cdr entry)))
        (expect entry :to-be-truthy)
        (expect (lookup-key map (kbd "q")) :to-equal #'neo/leave-current-application)
        ;; The map inherits `neo-application-mode-map', so its default
        ;; leave binding still resolves.
        (expect (lookup-key map (kbd "<S-up> <S-up>"))
                :to-equal #'neo/leave-current-application))))

  (it "installs nothing when :quit-keys is nil"
    (neo/application "NoQuitBuf"
      :setup (switch-to-buffer (get-buffer-create "*NoQuitBuf*"))
      :quit-keys nil)
    (setq persp-curr-name "Default")
    (setq neo--last-user-perspective nil)
    (delete-other-windows)
    (neo/app-noquitbuf)
    (with-current-buffer "*NoQuitBuf*"
      (expect (assq 'neo-application-mode minor-mode-overriding-map-alist)
              :to-be nil)))

  (it "targets the selected window's buffer when setup reverts current-buffer"
    ;; Mimic `elpaca-manager', whose `pop-to-buffer' runs inside
    ;; `with-current-buffer': the application window is selected but
    ;; `current-buffer' reverts to the caller once setup returns.
    (neo/application "RevertApp"
      :setup (set-window-buffer (selected-window) (get-buffer-create "*RevertApp*")))
    (setq persp-curr-name "Default")
    (setq neo--last-user-perspective nil)
    (delete-other-windows)
    (switch-to-buffer (get-buffer-create "*caller*"))
    (neo/app-revertapp)
    ;; The app buffer (selected window), not the caller, gets the bindings.
    (expect (assq 'neo-application-mode
                  (buffer-local-value 'minor-mode-overriding-map-alist
                                      (get-buffer "*RevertApp*")))
            :to-be-truthy)
    (expect (assq 'neo-application-mode
                  (buffer-local-value 'minor-mode-overriding-map-alist
                                      (get-buffer "*caller*")))
            :to-be nil)))