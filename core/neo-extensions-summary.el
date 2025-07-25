(require 'neo-extensions-detail)

(defvar-local neo-extensions--items nil
  "List of neo-extension structs.")
(defvar-local neo-extensions--positions nil
  "Alist mapping buffer positions to neo-extension objects.")
(defvar-local neo-extensions--current-index 0
  "Index of the currently selected item.")

(defface neo/extensions-highlight-face
  '((t (:inherit highlight :extend t)))
  "Face used to highlight the selected extension block.")

(defun neo/extensions-highlight-current ()
  "Highlight the currently selected extension and move point."
  (let ((inhibit-read-only t))
    (remove-overlays)
    (when (and (>= neo-extensions--current-index 0)
               (< neo-extensions--current-index (length neo-extensions--positions)))
      (let* ((entry (nth neo-extensions--current-index neo-extensions--positions))
             (range (car entry))
             (start (car range))
             (end (cdr range)))
        (goto-char start)
        (let ((ov (make-overlay start end)))
          (overlay-put ov 'face 'neo/extensions-highlight-face))))))

;; (defun neo/extensions-summary-open-buffer (extensions)
;;   "Display EXTENSIONS in the summary buffer."
;;   (let ((buf (get-buffer-create "*Neo Extensions*")))
;;     (with-current-buffer buf
;;       (neo-extensions-summary-mode)
;;       (let ((inhibit-read-only t))
;;         (erase-buffer)
;;         (setq neo-extensions--items extensions
;;               neo-extensions--positions nil
;;               neo-extensions--current-index 0)
;; 	(setq neo-extensions--positions
;; 	       (mapcar (lambda (ext)
;; 			 (let ((range (neo/render ext)))
;; 			   ;; Store (range . ext)
;; 			   (cons range ext)))
;; 		       extensions)))
;;       ;; Delay highlight until buffer is visible
;;       (pop-to-buffer buf)
;;       (neo/extensions-highlight-current))))

(defun neo/extensions-summary-open-buffer (extensions)
  "Display EXTENSIONS in the summary buffer."
  (let ((buf (get-buffer-create "*Neo Extensions*")))
    ;; Switch to the buffer *before* rendering so window-width is correct
    (pop-to-buffer buf)
    (with-current-buffer buf
      (neo-extensions-summary-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq neo-extensions--items extensions
              neo-extensions--positions nil
              neo-extensions--current-index 0)
        ;; Now that we're in the correct window context, this works
	(setq neo-extensions--positions
	      (mapcar (lambda (ext)
			(pcase-let ((`(,range ,overlay) (neo/render ext)))
			  (setf (neo/extension-summary-overlay ext) overlay)
			  (cons range ext)))
		      extensions))))))

      ;;   ;; (setq neo-extensions--positions
      ;;   ;;       (mapcar (lambda (ext)
      ;;   ;;                 (let ((range (neo/render ext)))
      ;;   ;;                   ;; Store (range . ext)
      ;;   ;;                   (cons range ext)))
      ;;   ;;               extensions)))
      ;; (neo/extensions-highlight-current))))

;; (defun neo/activate-extension-at-point ()
;;   "Open detailed view for the extension under point."
;;   (interactive)
;;   (let ((ext (get-text-property (point) 'neo-extension)))
;;     (if ext
;;         (neo/show-extension-details ext)
;;       (message "No extension at point."))))
(defun neo/activate-extension-at-point ()
  "Open detailed view for the extension under point, unless it's a real button."
  (interactive)
  (if (button-at (point))
      ;; Let the button's own action run (do nothing here)
      (message "Button action handled separately.")
    ;; Fallback to showing detailed view
    (let ((ext (get-text-property (point) 'neo-extension)))
      (if ext
          (neo/show-extension-details ext)
        (message "No extension at point.")))))

(defun neo/extensions-move (delta)
  "Move to the next/previous extension item by DELTA."
  (let* ((max-index (1- (length neo-extensions--positions)))
         (new-index (+ neo-extensions--current-index delta)))
    ;; Clamp new index to valid range
    (setq neo-extensions--current-index
          (max 0 (min max-index new-index)))
    (neo/extensions-highlight-current)))


(define-derived-mode neo-extensions-summary-mode special-mode "NeoExt-Summary"
  "Major mode for browsing Neo Extensions."
  (setq buffer-read-only t)
  (setq cursor-intangible-mode t)
  (setq-local cursor-type nil)
;  (blink-cursor-mode -1) ; probably not needed for the above
  (use-local-map (let ((map (make-sparse-keymap)))
                   (define-key map (kbd "n") (lambda () (interactive) (neo/extensions-move 1)))
                   (define-key map (kbd "p") (lambda () (interactive) (neo/extensions-move -1)))
                   ;; (define-key map (kbd "RET") #'neo/extensions-visit-current)
                   ;; (define-key map [mouse-1] #'neo/extensions-visit-current)
		   (define-key map (kbd "RET") #'neo/activate-extension-at-point)
		   (define-key map (kbd "<mouse-1>") #'neo/activate-extension-at-point)
                   map)))

(provide 'neo-extensions-summary)



;; (defgroup neo-extensions nil
;;   "Customization group for neo extensions."
;;   :group 'applications)

;; (defcustom neo-extensions-open-split t
;;   "Whether to open details in a split window."
;;   :type 'boolean
;;   :group 'neo-extensions)

;; (defun neo/render-extension (ext)
;;   "Render a neo-extension EXT at point and return point marker."
;;   (let ((start (point)))
;;     (when-let* ((emblem (neo-extension-emblem ext)))
;;       (when (stringp emblem)
;;         (insert-image (create-image emblem 'png t))
;;         (insert " ")))
;;     (insert (propertize (or (neo-extension-name ext) "Unnamed")
;;                         'face '(:weight bold :height 1.2)))
;;     (insert "\n\n")
;;     (when-let* ((desc (neo-extension-description ext)))
;;       (insert (propertize desc 'face '(:slant italic :height 0.95)))
;;       (insert "\n\n"))
;;     (dolist (key '(author forge user repo path))
;;       (when-let* ((val (slot-value ext key)))
;;         (insert (format "%-10s: %s\n" (capitalize (symbol-name key)) val))))
;;     (insert "\n" (make-string 60 ?─) "\n\n")
;;     (point-marker)))

;; (defun neo/parse-extension (plist)
;;   "Convert a PLIST into a `neo-extension` struct."
;;   (apply #'make-neo-extension plist))




;; (defun neo/extensions-visit-current ()
;;   "Visit the currently selected extension."
;;   (interactive)
;;   (let ((ext (cdr (nth neo-extensions--current-index neo-extensions--positions))))
;;     (when ext
;;       (run-hooks 'neo-extension-visit-hook)
;;       ;; Default: open buffer (split vs replace later)
;;       (message "Would open extension: %s" (neo-extension-name ext)))))

;; (defvar neo-extension-visit-hook nil
;;   "Hook called when an extension is selected. Called with the `neo-extension` struct.")

;; (define-derived-mode neo-extensions-summary-mode special-mode "NeoExt-Summary"
;;   "Major mode for browsing Neo Extensions."
;;   (setq buffer-read-only t)
;;   (use-local-map (let ((map (make-sparse-keymap)))
;;                    (define-key map (kbd "n") (lambda () (interactive) (neo/extensions-move 1)))
;;                    (define-key map (kbd "p") (lambda () (interactive) (neo/extensions-move -1)))
;;                    (define-key map (kbd "RET") #'neo/extensions-visit-current)
;;                    (define-key map [mouse-1] #'neo/extensions-visit-current)
;;                    map)))

;; (provide 'neo-extensions-summary)
