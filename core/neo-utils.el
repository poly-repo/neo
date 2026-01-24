;;; -*- lexical-binding: t -*-

(defun neo/set-emacs-source-directories ()
  (let (src)
    ;; Prefer configure-time prefix
    (when (and system-configuration-options
               (string-match "--prefix=\\([^ ]+\\)" system-configuration-options))
      (setq src (expand-file-name
                 "src/"
                 (match-string 1 system-configuration-options))))
    ;; Fallback: runtime invocation
    (unless (and src (file-directory-p src))
      (setq src (expand-file-name
                 "src/"
                 (file-name-directory
                  (directory-file-name invocation-directory)))))
    (when (file-directory-p src)
      (setq source-directory src
            find-function-C-source-directory src))))


(defun neo/with-ui-session (name &optional setup-fn teardown-fn)
  "Start a temporary UI session called NAME.

Switch to perspective NAME, run SETUP-FN in the selected window,
and return a quit function that restores the original perspective."
  (let ((orig-persp (persp-curr))
	(orig-header header-line-format))
    (persp-switch name)

    ;; ensure we act on the selected window
    (when setup-fn
      (funcall setup-fn))

    ;; now enforce single-window layout
    (delete-other-windows)

    (lambda ()
      (interactive)
      (when teardown-fn
        (funcall teardown-fn))
      (setq header-line-format orig-header)
      (persp-switch (persp-name orig-persp)))))
;; end of utility function

(defun neo/get-current-file-dir ()
  (or (and load-file-name (file-name-directory load-file-name))
      (and buffer-file-name (file-name-directory buffer-file-name))))
  
(defun neo/add-current-file-dir-to-load-path ()
  "Add the directory of the current file or buffer to `load-path`."
  (interactive)
  (let ((dir (neo/get-current-file-dir)))
    (when dir
      (add-to-list 'load-path dir)
      (message "Added %s to load-path" dir))))


(provide 'neo-utils)
