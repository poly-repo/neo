;;; neo-use-package-test.el --- Tests for neo/use-package -*- lexical-binding: t -*-

(require 'ert)

(unless (featurep 'dash)
  (defun -partition-before-pred (pred list)
    "Partition LIST before items matching PRED."
    (let ((result nil)
          (current nil))
      (dolist (item list)
        (when (and current (funcall pred item))
          (push (nreverse current) result)
          (setq current nil))
        (push item current))
      (when current
        (push (nreverse current) result))
      (nreverse result)))
(provide 'dash))

;; `neo--collect-package-sources' (in neo-use-package.el) reads
;; `neo/installation'/`neo/extension-slug' structs, defined in
;; neo-extensions.el; give it the same cache/config preamble
;; neo-extensions-test.el uses so `require' succeeds standalone.
(require 'neo-early-init-utils)
(defvar neo/cache-directory temporary-file-directory)
(defvar neo/config-directory temporary-file-directory)
(require 'neo-extensions)

(require 'neo-use-package)

(defconst neo-use-package-test--file
  "/tmp/neo/extensions/extensions/neo/test/neo-test.el")

(defvar neo/use-extensions t)

(ert-deftest neo/use-package-adds-ensure-by-default ()
  "Default Neo package declarations to a synchronous Elpaca ensure."
  (let ((load-file-name neo-use-package-test--file))
    (let ((expansion (prin1-to-string
                      (macroexpand-1 '(neo/use-package sample-package)))))
      (should (string-match-p ":ensure (:wait t)" expansion)))))

(ert-deftest neo/use-package-keeps-emacs-unensured ()
  "Keep built-in Emacs declarations out of Elpaca."
  (let ((load-file-name neo-use-package-test--file))
    (let ((expansion (prin1-to-string
                      (macroexpand-1 '(neo/use-package emacs :config (ignore))))))
      (should (string-match-p ":ensure nil" expansion)))))

(ert-deftest neo/prepare-use-package-form-disables-duplicate-installs ()
  "Avoid re-queueing duplicate package installs during replay."
  (let ((seen (make-hash-table :test 'equal)))
    (neo--prepare-use-package-form '(use-package sample-package :ensure t) seen)
    (let ((expansion (prin1-to-string
                      (neo--prepare-use-package-form
                       '(use-package sample-package :ensure t :config (ignore))
                       seen))))
      (should (string-match-p ":ensure nil" expansion)))))

(ert-deftest neo/merge-use-package-declarations-disjoint-sections ()
  "Merge disjoint sections from two sources without conflicts."
  (let* ((source-a (cons "neo" "extension-a"))
         (source-b (cons "neo" "extension-b"))
         (pairs (list (cons source-a (neo--sectioned-list->alist '(:init (foo-init))))
                      (cons source-b (neo--sectioned-list->alist '(:config (foo-config))))))
         (provenance (neo--merge-use-package-declarations 'foo pairs)))
    (should (equal (cdr (assoc :init (neo-package-provenance-merged-args-alist provenance)))
                   '((foo-init))))
    (should (equal (cdr (assoc :config (neo-package-provenance-merged-args-alist provenance)))
                   '((foo-config))))
    (should (null (neo-package-provenance-conflicts provenance)))))

(ert-deftest neo/merge-use-package-declarations-custom-same-value-dedupes ()
  "Same `:custom' variable/value pair from two sources dedupes without conflict."
  (let* ((source-a (cons "neo" "questionable-defaults"))
         (source-b (cons "neo" "compsel"))
         (pairs (list (cons source-a (neo--sectioned-list->alist
                                       '(:custom (read-extended-command-predicate #'identity))))
                      (cons source-b (neo--sectioned-list->alist
                                       '(:custom (read-extended-command-predicate #'identity))))))
         (provenance (neo--merge-use-package-declarations 'emacs pairs)))
    (should (equal (cdr (assoc :custom (neo-package-provenance-merged-args-alist provenance)))
                   '((read-extended-command-predicate #'identity))))
    (should (null (neo-package-provenance-conflicts provenance)))))

(ert-deftest neo/merge-use-package-declarations-custom-conflicting-value ()
  "Differing `:custom' values for the same variable keep the first and warn."
  (let* ((source-a (cons "neo" "extension-a"))
         (source-b (cons "neo" "extension-b"))
         (pairs (list (cons source-a (neo--sectioned-list->alist '(:custom (some-var 1))))
                      (cons source-b (neo--sectioned-list->alist '(:custom (some-var 2))))))
         (provenance (neo--merge-use-package-declarations 'foo pairs)))
    (should (equal (cdr (assoc :custom (neo-package-provenance-merged-args-alist provenance)))
                   '((some-var 1))))
    (should (= (length (neo-package-provenance-conflicts provenance)) 1))
    (let ((conflict (car (neo-package-provenance-conflicts provenance))))
      (should (eq (neo-merge-conflict-sub-key conflict) 'some-var))
      (should (equal (neo-merge-conflict-kept-value conflict) '(some-var 1)))
      (should (equal (neo-merge-conflict-dropped-value conflict) '(some-var 2)))
      (should (equal (neo-merge-conflict-kept-source conflict) source-a))
      (should (equal (neo-merge-conflict-dropped-source conflict) source-b)))))

(ert-deftest neo/merge-use-package-declarations-ensure-default-vs-real ()
  "A framework-default `:ensure' loses to a real recipe from another source."
  (let* ((source-a (cons "neo" "ai-buddy"))
         (source-b (cons "neo" "terminal"))
         (pairs (list (cons source-a (neo--sectioned-list->alist
                                       '(:ensure (:host github :repo "akermu/emacs-libvterm"))))
                      (cons source-b (neo--sectioned-list->alist
                                       '(:ensure (:wait t) :custom (vterm-max-scrollback 100000))))))
         (provenance (neo--merge-use-package-declarations 'vterm pairs)))
    (should (equal (cdr (assoc :ensure (neo-package-provenance-merged-args-alist provenance)))
                   '((:host github :repo "akermu/emacs-libvterm"))))
    (should (equal (cdr (assoc :custom (neo-package-provenance-merged-args-alist provenance)))
                   '((vterm-max-scrollback 100000))))
    (should (null (neo-package-provenance-conflicts provenance)))))

(ert-deftest neo/merge-use-package-declarations-ensure-both-default-collapses ()
  "Two default-only `:ensure' declarations collapse into one, no conflict.
Reconstructs the historical omega-45pw `transient' race: both
declarations only carried the framework-injected default `:ensure', so
merging removes the race by construction instead of relying on Elpaca
queue timing."
  (let* ((source-a (cons "neo" "better-git"))
         (source-b (cons "neo" "programming-foundation"))
         (pairs (list (cons source-a (neo--sectioned-list->alist '(:ensure (:wait t))))
                      (cons source-b (neo--sectioned-list->alist '(:ensure (:wait t))))))
         (provenance (neo--merge-use-package-declarations 'transient pairs)))
    (should (equal (cdr (assoc :ensure (neo-package-provenance-merged-args-alist provenance)))
                   (list neo--use-package-default-ensure)))
    (should (null (neo-package-provenance-conflicts provenance)))))

(ert-deftest neo/merge-use-package-declarations-ensure-conflicting-recipes ()
  "Two differing real `:ensure' recipes keep the first and warn."
  (let* ((source-a (cons "neo" "better-git"))
         (source-b (cons "neo" "programming-foundation"))
         (pairs (list (cons source-a (neo--sectioned-list->alist '(:ensure (:host github :repo "a/a"))))
                      (cons source-b (neo--sectioned-list->alist '(:ensure (:host github :repo "b/b"))))))
         (provenance (neo--merge-use-package-declarations 'transient pairs)))
    (should (equal (cdr (assoc :ensure (neo-package-provenance-merged-args-alist provenance)))
                   '((:host github :repo "a/a"))))
    (should (= (length (neo-package-provenance-conflicts provenance)) 1))
    (let ((conflict (car (neo-package-provenance-conflicts provenance))))
      (should (eq (neo-merge-conflict-section conflict) :ensure))
      (should (equal (neo-merge-conflict-kept-value conflict) '(:host github :repo "a/a")))
      (should (equal (neo-merge-conflict-dropped-value conflict) '(:host github :repo "b/b"))))))

(ert-deftest neo/collect-package-sources-groups-by-name-in-order ()
  "Group queued `use-package' forms by package name in dependency order."
  (let* ((slug-a (make-neo/extension-slug :publisher "neo" :name "extension-a"))
         (slug-b (make-neo/extension-slug :publisher "neo" :name "extension-b"))
         (installation-a (make-neo/installation :extension-slug slug-a))
         (installation-b (make-neo/installation :extension-slug slug-b))
         (installed-map (make-hash-table :test 'equal)))
    (puthash "neo:extension-a" installation-a installed-map)
    (puthash "neo:extension-b" installation-b installed-map)
    (let* ((enabled-packages
            (list (cons (cons "neo" "extension-a")
                        (list '(use-package foo :init (foo-init-a))))
                  (cons (cons "neo" "extension-b")
                        (list '(use-package foo :init (foo-init-b))
                              '(use-package bar :config (bar-config))))))
           (sorted-slugs (list "neo:extension-a" "neo:extension-b"))
           (grouped (neo--collect-package-sources sorted-slugs installed-map enabled-packages)))
      (should (equal (mapcar #'car grouped) '(foo bar)))
      (let ((foo-sources (mapcar #'car (cdr (assoc 'foo grouped)))))
        (should (equal foo-sources (list (cons "neo" "extension-a") (cons "neo" "extension-b"))))))))

(ert-deftest neo/format-package-provenance-mentions-sources-and-ensure ()
  "Formatted provenance mentions all contributing sources and the winning `:ensure'."
  (let* ((source-a (cons "neo" "ai-buddy"))
         (source-b (cons "neo" "terminal"))
         (pairs (list (cons source-a (neo--sectioned-list->alist
                                       '(:ensure (:host github :repo "akermu/emacs-libvterm"))))
                      (cons source-b (neo--sectioned-list->alist
                                       '(:ensure (:wait t) :custom (vterm-max-scrollback 100000))))))
         (provenance (neo--merge-use-package-declarations 'vterm pairs))
         (report (neo--format-package-provenance provenance)))
    (should (string-match-p "neo:ai-buddy" report))
    (should (string-match-p "neo:terminal" report))
    (should (string-match-p "akermu/emacs-libvterm" report))))

(ert-deftest neo/format-package-provenance-reports-single-declaration ()
  "An unduplicated package reports that no merge occurred."
  (let* ((source-a (cons "neo" "solo"))
         (pairs (list (cons source-a (neo--sectioned-list->alist '(:config (solo-config))))))
         (provenance (neo--merge-use-package-declarations 'solo-package pairs))
         (report (neo--format-package-provenance provenance)))
    (should (string-match-p "single declaration" report))
    (should (string-match-p "neo:solo" report))))

(provide 'neo-use-package-test)
;;; neo-use-package-test.el ends here
