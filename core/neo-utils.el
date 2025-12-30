;;; -*- lexical-binding: t -*-

;; TODO this will go into utility
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


(provide 'neo-utils)
