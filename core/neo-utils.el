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

(provide 'neo-utils)
