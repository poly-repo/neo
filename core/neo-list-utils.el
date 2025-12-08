;(require 'dash)

					;(use-package dash :ensure t)
;(package-install 'dash)
(require 'dash)

;;; TODO maybe keyword-group is better than sectioned-list

(defun neo--sectioned-list->alist (data)
  "Convert DATA (keyword-separated groups) into an alist.
Uses `-partition-before-pred` from `dash` to split DATA on keywords.
Each partition becomes (KEY . VAL), where VAL is
- a list of the following forms when present, or
- the symbol `neo/no-args` when the key had no arguments.

Returns an empty list when DATA is empty."
  (mapcar (lambda (part)
            (let ((key (car part))
                  (vals (cdr part)))
              (cons key (if vals vals 'neo/no-args))))
          (or (-partition-before-pred #'keywordp data) '())))

(defun neo--alist->sectioned-list (alist)
  "Convert an ALIST of grouped keyword sections back into a flat sectioned list.

Each key maps to either a list of forms or the symbol `neo/no-args`.
Produces a flat list like:
  (:key form1 form2 ... :next-key ...)"
  (apply #'append
         (mapcar (lambda (entry)
                   (let ((key (car entry))
                         (forms (cdr entry)))
                     (if (eq forms 'neo/no-args)
                         (list key)       ; key with no args
                       (cons key forms))))  ; key with args
                 alist)))


(provide 'neo-list-utils)
