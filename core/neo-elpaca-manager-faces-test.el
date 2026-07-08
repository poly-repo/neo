;;; neo-elpaca-manager-faces-test.el --- Tests for neo-elpaca-manager-faces -*- lexical-binding: t -*-

(require 'buttercup)
(require 'tabulated-list)
(require 'neo-elpaca-manager-faces)

(describe "neo-elpaca-manager-faces"

  (it "faces non-Package columns and leaves Package untouched"
    (with-temp-buffer
      (tabulated-list-mode)
      (setq tabulated-list-format
            [("Package" 10 t) ("Description" 20 t) ("Date" 10 t) ("Source" 10 t)]
            tabulated-list-use-header-line nil)
      (tabulated-list-init-header)
      (tabulated-list-print-fake-header)
      (tabulated-list-print-entry
       'pkg1 ["foo" "a description" "2026-01-01" "melpa"])
      (neo/elpaca-manager--apply-column-faces (point-min) (point-max))

      (goto-char (point-min))
      (forward-line 1) ; past the fake header, onto the data row
      (let* ((line-end (pos-eol))
             (pkg-pos (point))
             (desc-pos (next-single-property-change
                        pkg-pos 'tabulated-list-column-name nil line-end)))
        (expect (get-text-property pkg-pos 'face) :to-be nil)
        (let ((f (get-text-property desc-pos 'face)))
          (expect (memq 'neo/elpaca-manager-description-column
                        (if (listp f) f (list f)))
                  :to-be-truthy)))))

  (it "registers a buffer-local remap for the header face on the mode hook"
    (with-temp-buffer
      (neo/elpaca-manager--setup-faces)
      (expect (assq 'tabulated-list-fake-header face-remapping-alist)
              :to-be-truthy))))
