(require 'cl-lib)

(defun neo/xlfd-parse (xlfd)
  "Parse an XLFD string XLFD and return an alist of fields."
  (when (and (stringp xlfd) (string-prefix-p "-" xlfd))
    (let* ((parts (split-string xlfd "-" t))
           (keys '(:foundry :family :weight :slant :setwidth :addstyle
                            :pixel_size :point_size :res_x :res_y
                            :spacing :avgwidth :registry :encoding)))
      (cl-mapcar #'cons keys parts))))

(defun neo/xlfd-to-font-spec (xlfd)
  "Convert XLFD string XLFD into a `font-spec` object."
  (when-let ((fields (xlfd-parse xlfd)))
    (let* ((family (alist-get :family fields))
           (weight (alist-get :weight fields))
           (slant  (alist-get :slant fields))
           (pt     (alist-get :point_size fields))
           ;; point_size is often in decipoints
           (size (when (and pt (string-match-p "^[0-9]+$" pt))
                   (/ (string-to-number pt) 10.0)))
           (weight-val (cond
                        ((string-match-p "bold" (downcase weight)) 'bold)
                        ((string-match-p "medium" (downcase weight)) 'medium)
                        ((string-match-p "light" (downcase weight)) 'light)
                        (t nil)))
           (slant-val (cond
                       ((string-match-p "italic\\|oblique" (downcase slant)) 'italic)
                       (t 'normal))))
      (font-spec :family family
                 :weight weight-val
                 :slant slant-val
                 :size size))))

(defun neo/xlfd-to-readable-font-string (xlfd)
  "Return a human-readable string from an XLFD for `set-frame-font`."
  (when-let* ((spec (xlfd-to-font-spec xlfd))
              (family (font-get spec :family)))
    (format "%s-%s-%s-%s"
            family
            (or (symbol-name (font-get spec :weight)) "normal")
            (or (symbol-name (font-get spec :slant)) "normal")
            (or (font-get spec :size) ""))))

(require 'tabulated-list)

(defcustom neo/font-browser-sample-text
  "The quick brown fox jumps over the lazy dog 0123456789"
  "Sample text shown for each font in the browser."
  :type 'string)

(defcustom neo/font-browser-sample-height 120
  "Sample font height (Emacs :height) shown in the sample column.
Typical values: 100 (default size), 120 (larger), 80 (smaller)."
  :type 'integer)

(defvar neo/font-browser--filter nil
  "Regexp filter applied to font family names (or nil for none).")

(defun neo/font-browser--make-entry (family)
  "Return a tabulated-list entry for font FAMILY."
  (let* ((sample-face `(:family ,family :height ,neo/font-browser-sample-height))
         (sample (propertize neo/font-browser-sample-text 'face sample-face))
         ;; ID = family symbol for easy retrieval
         (id (intern family)))
    (list id (vector family sample))))

(defun neo/font-browser--build-entries ()
  "Build entries for `tabulated-list-entries' from `font-family-list' and the filter."
  (let ((families (sort (copy-sequence (font-family-list)) #'string-lessp)))
    (when neo/font-browser--filter
      (setq families (cl-remove-if-not (lambda (f) (string-match-p neo/font-browser--filter f)) families)))
    (mapcar #'neo/font-browser--make-entry families)))

(define-derived-mode neo/font-browser-mode tabulated-list-mode "Font-Browser"
  "Major mode for browsing system fonts with samples."
  (setq tabulated-list-format [("Family" 30 t)
                               ("Sample" 0 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-entries #'neo/font-browser--build-entries)
  (tabulated-list-init-header))

;;;###autoload
(defun neo/font-browser (&optional filter)
  "Open the font browser.
With optional prefix (or FILTER string) ask for a regexp to filter font family names."
  (interactive (list (when current-prefix-arg
                       (read-string "Filter (regexp) for font families: "))))
  (setq neo/font-browser--filter (and filter (if (string= filter "") nil filter)))
  (let ((buf (get-buffer-create "*Font Browser*")))
    (with-current-buffer buf
      (neo/font-browser-mode)
      (tabulated-list-print t))
    (pop-to-buffer buf)))

;; Keybindings and actions in the browser

(define-key neo/font-browser-mode-map (kbd "g") #'neo/font-browser-refresh)
(define-key neo/font-browser-mode-map (kbd "f") #'neo/font-browser-set-filter)
(define-key neo/font-browser-mode-map (kbd "q") #'quit-window)
(define-key neo/font-browser-mode-map (kbd "RET") #'neo/font-browser-describe-font-at-point)

(defun neo/font-browser-refresh ()
  "Refresh the font browser entries (re-evaluates `font-family-list`)."
  (interactive)
  (tabulated-list-revert))

(defun neo/font-browser-set-filter (regexp)
  "Set a REGEXP filter for font family names; empty => clear filter."
  (interactive (list (read-string "Filter (regexp, empty to clear): " (or neo/font-browser--filter ""))))
  (setq neo/font-browser--filter (and (not (string= regexp "")) regexp))
  (neo/font-browser-refresh))
 
(defun neo/font-browser-describe-font-at-point ()
  "Describe the font family on the current row via `describe-font'."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (family (and id (symbol-name id))))
    (if (not family)
        (message "No font family on this line.")
      ;; Try to construct a font-spec and call describe-font. If that fails, fall back to describe-face.
      (condition-case err
          (describe-font (font-spec :family family))
        (error
         (message "Couldn't describe font %s: %s. Trying describe-face fallback." family err)
         (describe-face `(:family ,family)))))))
