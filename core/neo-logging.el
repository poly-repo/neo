;;; neo-log.el --- Structured logging UI with details buffer -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ring)
(require 'tabulated-list)
(require 'subr-x)

(defface neo/log-more-face
  '((t :inherit font-lock-doc-face :weight bold))
  "Face for the details indicator.")

(defface neo/log-detail-key-face
  '((t :weight bold :inherit font-lock-keyword-face))
  "Face for keys in the main section of log details.")

(defface neo/log-detail-subkey-face
  '((t :weight bold :inherit font-lock-constant-face))
  "Face for keys in the Details section of log details.")

(cl-defstruct neo/log-entry
  time        ;; float-time
  level       ;; symbol: trace debug info warn error fatal
  source      ;; string or symbol
  message     ;; formatted string
  details)    ;; plist or nil

(defcustom neo/log-max-entries 1000
  "Maximum number of log entries to keep in memory."
  :type 'integer)

(defvar neo/log-entries (make-ring neo/log-max-entries)
  "Ring of `neo/log-entry` objects.")

(defun neo/log--store (entry)
  (ring-insert neo/log-entries entry))

(defun neo/log--push (level source fmt &rest args)
  "Push a new log entry."
  (let ((details (plist-get args :details)))
    (let ((entry (make-neo/log-entry
                  :time (float-time)
                  :level level
                  :source source
                  :message (apply #'format fmt args)
                  :details details)))
      (neo/log--store entry)
      entry)))

(defun neo/log--push (level source fmt &rest args)
  "Push a new log entry with LEVEL, SOURCE, formatted FMT and ARGS.
If ARGS contains a plist with :details, it is extracted."
  (let* ((details (when args
                    (let ((last-arg (car (last args))))
                      (when (and (listp last-arg)
                                 (keywordp (car last-arg)))
                        last-arg))))
         (fmt-args (if details
                       (butlast args)
                     args))
         (entry (make-neo/log-entry
                 :time (float-time)
                 :level level
                 :source source
                 :message (apply #'format fmt fmt-args)
                 :details details)))
    (neo/log--store entry)
    entry))


(dolist (level '(trace debug info warn error fatal))
  (let ((fn (intern (format "neo/log-%s" level))))
    (eval `(defun ,fn (source fmt &rest args)
             (apply #'neo/log--push ',level source fmt args)))))

(defvar neo/log-filter-levels nil)
(defvar neo/log-filter-source nil)
(defvar neo/log-filter-text nil)

;; TODO: map levels to integers and use a simple comparison as a
;; decision criteria
(defun neo/log--entry-visible-p (entry)
  (and
   (or (null neo/log-filter-levels)
       (memq (neo/log-entry-level entry) neo/log-filter-levels))
   (or (null neo/log-filter-source)
       (equal (neo/log-entry-source entry) neo/log-filter-source))
   (or (null neo/log-filter-text)
       (string-match-p
        (regexp-quote neo/log-filter-text)
        (neo/log-entry-message entry)))))

(defun neo/log--format-time (time)
  (format-time-string "%H:%M:%S"
                      (seconds-to-time time)))

(defun neo/log--level-string (level)
  (upcase (symbol-name level)))

(defun neo/log--tabulated-entries ()
  "Return tabulated-list-entries from the log ring."
  (let ((rows '()))
    (dotimes (i (ring-length neo/log-entries))
      (let ((e (ring-ref neo/log-entries i)))
        (when (neo/log--entry-visible-p e)
          (setq rows
                (cons
                 (list e
                       (vector
                        (if (neo/log-entry-details e)
                            (propertize "ℹ️"
                                        'face 'neo/log-more-face
                                        'help-echo "This log entry has additional details")
                          "")
                        (neo/log--format-time (neo/log-entry-time e))
                        (neo/log--level-string (neo/log-entry-level e))
                        (format "%s" (neo/log-entry-source e))
                        (neo/log-entry-message e)))
                 rows)))))
    rows))
;    (nreverse rows)))

(defun neo/log--display-entry (entry)
  "Display a detailed view of log ENTRY."
  (let ((buf (get-buffer-create "*Neo Log Details*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)

        (let ((metadata `(("Time" . ,(neo/log--format-time (neo/log-entry-time entry)))
                          ("Level" . ,(neo/log--level-string (neo/log-entry-level entry)))
                          ("Source" . ,(neo/log-entry-source entry)))))
          (dolist (pair metadata)
            (insert (propertize (format "%-8s" (car pair))
                                'face 'neo/log-detail-key-face))
            (insert (format "%s\n" (cdr pair)))))

        (insert (propertize "\nMessage:\n" 'face 'neo/log-detail-key-face))
        (insert (format "%s\n\n" (neo/log-entry-message entry)))

        (let ((details (neo/log-entry-details entry)))
          (if (and details (not (null details)))
              (progn
                (insert (propertize "Details:\n" 'face 'neo/log-detail-key-face))
                (let ((max-key-width (apply #'max
                                            (mapcar (lambda (k) (length (symbol-name k)))
                                                    (cl-loop for (k _) on details by #'cddr collect k)))))
                  (cl-loop for (k v) on details by #'cddr do
                           (insert (propertize (format (format "    %%-%ds" max-key-width)
                                                       (symbol-name k))
                                               'face 'neo/log-detail-subkey-face))
                           (insert (format " %s\n" v))))))))

      (goto-char (point-min))
      (special-mode)
      (pop-to-buffer buf))))

(defun neo/show-log-details (entry)
  "Show a log ENTRY in the details buffer."
  (interactive)
  (neo/log--display-entry entry))

(defun neo/log--show-details ()
  "Show details for the log entry at point in a tabulated list."
  (interactive)
  (let ((entry (tabulated-list-get-id)))
    (unless (neo/log-entry-p entry)
      (error "Invalid tabulated-list ID: %S" entry))
    (neo/log--display-entry entry)))

(define-derived-mode neo/log-mode tabulated-list-mode "Neo-Log"
  "Major mode for viewing Neo logs."
  (setq tabulated-list-format
        [("More"  4 t)
         ("Time"   8  t)
         ("Level"  7  t)
         ("Source" 20 t)
         ("Message" 0 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Time" . nil))
  (add-hook 'tabulated-list-revert-hook
            #'neo/log-refresh nil t)
  (tabulated-list-init-header))

(defun neo/log-refresh-buffer ()
  "Refresh the Neo log buffer contents."
  (setq tabulated-list-entries (neo/log--tabulated-entries))
  (tabulated-list-print t))

(defun neo/log-refresh ()
  (setq tabulated-list-entries (neo/log--tabulated-entries)))

;;; TODO make a variant of this that show logs if any are Warnings or
;;; above (or make the start level configurable)
(defun neo/log-show ()
  "Show Neo log buffer."
  (interactive)
  (let ((buf (get-buffer-create "*Neo Log*")))
    (with-current-buffer buf
      (neo/log-mode)
      (neo/log-refresh-buffer))
    (pop-to-buffer buf)))

(defun neo/log-filter-by-level ()
  (interactive)
  (setq neo/log-filter-levels
        (let* ((levels '(trace debug info warn error fatal))
               (choice (completing-read-multiple
                        "Levels (empty = all): "
                        (mapcar #'symbol-name levels)
                        nil t)))
          (when choice
            (mapcar #'intern choice))))
  (tabulated-list-revert))

(defun neo/log-filter-by-source ()
  (interactive)
  (let ((s (read-string "Source (empty = all): ")))
    (setq neo/log-filter-source
          (unless (string-empty-p s) s)))
  (tabulated-list-revert))

(defun neo/log-filter-by-text ()
  (interactive)
  (let ((s (read-string "Message contains (empty = all): ")))
    (setq neo/log-filter-text
          (unless (string-empty-p s) s)))
  (tabulated-list-revert))

(defun neo/log-clear-filters ()
  (interactive)
  (setq neo/log-filter-levels nil
        neo/log-filter-source nil
        neo/log-filter-text nil)
  (tabulated-list-revert))

(let ((map neo/log-mode-map))
  (define-key map (kbd "RET") #'neo/log--show-details)
  (define-key map (kbd "f l") #'neo/log-filter-by-level)
  (define-key map (kbd "f s") #'neo/log-filter-by-source)
  (define-key map (kbd "f t") #'neo/log-filter-by-text)
  (define-key map (kbd "f c") #'neo/log-clear-filters)
  (define-key map (kbd "g")   #'tabulated-list-revert))

(neo/log-info 'core "Logging ready")

(provide 'neo-logging)
