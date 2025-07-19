(defun neo/show-extension-details (ext)
  "Open a buffer showing detailed information for EXT."
  (let ((buf (get-buffer-create "*Neo Extension Detail*")))
    (with-current-buffer buf
      (neo-extension-detail-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (neo/render-details ext)
        (goto-char (point-min))))
    (pop-to-buffer buf)))

;; (defun neo/quit-detail-and-return ()
;;   "Quit the detail view and return to the summary buffer."
;;   (interactive)
;;   (kill-buffer)
;;   (when-let ((buf (get-buffer "*Neo Extensions*")))
;;     (pop-to-buffer buf)))

(defun neo/quit-detail-and-return ()
  "Quit the detail view and return to the summary buffer, reusing its window if available."
  (interactive)
  (let ((summary-window (get-buffer-window "*Neo Extensions*" t)))
    (kill-buffer)
    (if summary-window
        (select-window summary-window)
      (when-let ((buf (get-buffer "*Neo Extensions*")))
        (pop-to-buffer buf)))))

(define-derived-mode neo-extension-detail-mode special-mode "Neo-Extension-Detail"
  "Major mode for viewing a single Neo extension's details."
  (setq buffer-read-only t)
  (setq-local cursor-type nil)
  (define-key neo-extension-detail-mode-map (kbd "q") #'neo/quit-detail-and-return))

(provide 'neo-extensions-detail)
