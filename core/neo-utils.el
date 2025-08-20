(require 'cl-lib)

(defmacro neo/write-early-init-config! (&rest vars)
  "Write current values of VARS to the early-init config file for this Emacs instance.
The file is stored in ~/.config/neo/INSTANCE-NAME-early-init-config.el."
  `(let* ((instance-name (neo/get-emacs-instance-name))
          (file-name (format "%s-early-init-config.el" instance-name))
          (target-dir (expand-file-name "neo" (or (getenv "XDG_CONFIG_HOME") "~/.config")))
          (target-file (expand-file-name file-name target-dir)))
     (make-directory target-dir t)
     (with-temp-file target-file
       (insert ";; Auto-generated early-init settings\n\n")
       ,@(mapcar (lambda (var)
                   `(progn
                      (prin1 '(setq ,var ,(symbol-value var)) (current-buffer))
                      (insert "\n")))
                 vars))
     (message "neo/hacking: wrote early-init config to %s" target-file)))

(defmacro neo/hacking-write-early-init-config! (&rest specs)
  "Write early-init config for this Emacs instance.

Accepts:
- bare symbols (variables) → emits (setq VAR VAL)
- function calls (FUNC)    → emits (FUNC VAL), where VAL is the current value of FUNC variable
- pairs (FUNC . VAL-FORM) → emits (FUNC VAL-FORM) for custom value resolution

Writes to ~/.config/neo/INSTANCE-early-init-config.el"
  `(let* ((instance-name (neo/get-emacs-instance-name))
          (file-name (format "%s-early-init-config.el" instance-name))
          (target-dir (expand-file-name "neo" (or (getenv "XDG_CONFIG_HOME") "~/.config")))
          (target-file (expand-file-name file-name target-dir)))
     (make-directory target-dir t)
     (with-temp-file target-file
       (insert ";; Auto-generated early-init settings\n\n")
       ,@(mapcar
          (lambda (spec)
	    (message (format "DOING %s" spec))
            (cond
             ;; case: (VAR)
             ((symbolp spec)
              `(progn
                 (prin1 '(setq ,spec ,(symbol-value spec)) (current-buffer))
                 (insert "\n")))
             ;; case: ((FUNC . VAL-FORM))
             ((and (consp spec)
                   (symbolp (car spec))) ; (FOO . ...)
              `(progn
                 (prin1 '(,(car spec) ,(eval (cdr spec))) (current-buffer))
                 (insert "\n")))
             ;; case: (FUNC) treated as (FUNC VAR)
             ((and (listp spec)
                   (= (length spec) 1)
                   (symbolp (car spec)))
              (let* ((fn (car spec)))
                (unless (boundp fn)
                  (error "No variable named `%s` found for function `%s`" fn fn))
                `(progn
                   (prin1 '(,fn ,(symbol-value fn)) (current-buffer))
                   (insert "\n"))))
             (t `(insert ";; Unsupported spec format\n"))))
          specs))
     (message "neo/hacking: wrote early-init config to %s" target-file)))

(defmacro neo/hacking-write-early-init-config! (&rest specs)
  "Write early-init config for this Emacs instance.

Accepts:
- SYMBOL (variable) → emits (setq SYMBOL VALUE)
- (FUNC)            → emits (FUNC VALUE), using value from matching variable
- (FUNC . FORM)     → emits (FUNC FORM)

Writes to ~/.config/neo/INSTANCE-early-init-config.el"
  `(let* ((instance-name (neo/get-emacs-instance-name))
          (file-name (format "%s-early-init-config.el" instance-name))
          (target-dir (expand-file-name "neo" (or (getenv "XDG_CONFIG_HOME") "~/.config")))
          (target-file (expand-file-name file-name target-dir)))
     (make-directory target-dir t)
     (with-temp-file target-file
       (insert ";; Auto-generated early-init settings\n\n")
       ,@(mapcar
          (lambda (spec)
            (cond
             ;; (FUNC) → check for function first, then use associated variable value
             ((and (listp spec)
                   (= (length spec) 1)
                   (symbolp (car spec)))
              (let ((fn (car spec)))
                (cond
                 ((and (fboundp fn) (boundp fn))
                  `(progn
                     (prin1 '(,fn ,(symbol-value fn)) (current-buffer))
                     (insert "\n")))
                 ((fboundp fn)
                  (error "Function `%s` exists but no bound variable to determine argument" fn))
                 (t
                  (error "Symbol `%s` is not a function" fn)))))
             ;; (FUNC . EXPR) → emit as-is
             ((and (consp spec)
                   (symbolp (car spec)))
              `(progn
                 (prin1 '(,(car spec) ,(eval (cdr spec))) (current-buffer))
                 (insert "\n")))


             ;; SYMBOL → treat as variable and emit (setq SYMBOL VALUE)
             ((symbolp spec)
              `(progn
                 (prin1 '(setq ,spec ,(symbol-value spec)) (current-buffer))
                 (insert "\n")))

             (t
              `(insert ";; Unsupported spec format\n"))))
          specs))
     (message "neo/hacking: wrote early-init config to %s" target-file)))

(defmacro neo/hacking-write-early-init-config! (&rest specs)
  "Write early-init config for this Emacs instance.

Accepts:
- SYMBOL (variable) → emits (setq SYMBOL VALUE)
- (FUNC)            → emits (FUNC VALUE), using value from matching variable
- (FUNC . FORM)     → emits (FUNC FORM)

Writes to ~/.config/neo/INSTANCE-early-init-config.el"
  `(let* ((instance-name (neo/get-emacs-instance-name))
          (file-name (format "%s-early-init-config.el" instance-name))
          (target-dir (expand-file-name "neo" (or (getenv "XDG_CONFIG_HOME") "~/.config")))
          (target-file (expand-file-name file-name target-dir)))
     (make-directory target-dir t)
     (with-temp-file target-file
       (insert ";; Auto-generated early-init settings\n\n")
       ,@(mapcar
          (lambda (spec)
            (cond
	     ((and (consp spec)
		   (symbolp (car spec)))
	      (let* ((fn (car spec))
		     (form (cdr spec)))
		(cond
		 ;; (FUNC) or (FUNC . nil): evaluate (if fn +1 -1) at macro expansion
		 ((null form)
		  (unless (boundp fn)
		    (error "Cannot derive value for `%s`, no variable bound" fn))
		  (let ((value (if (symbol-value fn) +1 -1)))
		    `(progn
		       (prin1 '(,fn ,value) (current-buffer))
		       (insert "\n"))))

		 ;; (FUNC . FORM): evaluate FORM
		 (t
		  `(progn
		     (prin1 '(,fn ,(eval form)) (current-buffer))
		     (insert "\n"))))))
             ;; SYMBOL → treat as variable and emit (setq SYMBOL VALUE)
             ((symbolp spec)
              `(progn
                 (prin1 '(setq ,spec ,(symbol-value spec)) (current-buffer))
                 (insert "\n")))
             (t
              `(insert ";; Unsupported spec format\n"))))
          specs))
     (message "neo/hacking: wrote early-init config to %s" target-file)))

(defun neo/path-join (&rest segments)
  "Join SEGMENTS into a single normalized path.
Handles redundant slashes and expands `~` to the home directory."
  (let ((path (car segments)))
    (dolist (segment (cdr segments) path)
      (setq path (expand-file-name segment (file-name-as-directory path))))))

(defun neo/ensure-directory-exists (dir)
  "Ensure DIR exists. Create it and its parents if necessary.
Returns the absolute expanded path to DIR."
  (let ((full-dir (expand-file-name dir)))
    (unless (file-directory-p full-dir)
      (make-directory full-dir t))
    full-dir))

(defun neo/native-cache (package)
  (let ((dir (neo/path-join "~/.cache" "neo" emacs-version package)))
    (neo/ensure-directory-exists dir)))

(require 'color)

(defun neo/theme-dark-p ()
  "Return t if current theme has a dark background, nil if it's light."
  (let* ((bg (face-attribute 'default :background nil))
         (rgb (color-name-to-rgb bg)))
    (when rgb
      ;; Calculate relative luminance (per W3C WCAG 2.0)
      ;; Formula: 0.2126*R + 0.7152*G + 0.0722*B
      (let ((luminance (apply #'+
                              (cl-mapcar (lambda (channel weight)
                                           (* (expt channel 2.2) weight))
                                         rgb '(0.2126 0.7152 0.0722)))))
        (< luminance 0.5)))))

(require 'color)

(defun neo/compute-contrasting-colors ()
  "Return a cons cell (NEW-BG . NEW-FG) that contrasts with the current theme background."
  (let* ((bg (face-attribute 'default :background nil))
         (bg-rgb (color-name-to-rgb bg))
         (luminance (when bg-rgb
                      (apply #'+
                             (cl-mapcar (lambda (channel weight)
                                          (* (expt channel 2.2) weight))
                                        bg-rgb '(0.2126 0.7152 0.0722)))))
         ;; Choose a contrasting background color
         (new-bg (if (and luminance (< luminance 0.5))
                     "#f5f5f5"  ; light gray
                   "#202020")) ; dark gray
         ;; Choose a foreground based on new background
         (new-fg (if (string= new-bg "#f5f5f5")
                     "#202020"  ; dark text
                   "#f5f5f5"))) ; light text
    (cons new-bg new-fg)))

;; TODO doesn't really belong here, but should be available when
;; programming extensions are loaded. Fix when dependencies do work.
(defun neo/eglot-set-server (modes server-command)
  "Install SERVER-COMMAND for MODES in `eglot-server-programs`.

MODES is a symbol or a list of symbols.
SERVER-COMMAND is a list like (\"pyright-langserver\" \"--stdio\")."
  (let ((mode-list (if (listp modes) modes (list modes))))
    (with-eval-after-load 'eglot
      ;; Remove any existing entries that mention these modes
      (setq eglot-server-programs
            (cl-remove-if
             (lambda (cell)
               (let ((k (car-safe cell)))
                 (cond
                  ((symbolp k) (memq k mode-list))
                  ((and (consp k) (cl-intersection k mode-list))))))
             eglot-server-programs))
      ;; Add a single grouped entry so either mode hits this server
      (push (cons mode-list server-command) eglot-server-programs))))

(provide 'neo-utils)
