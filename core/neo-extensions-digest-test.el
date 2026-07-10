;;; neo-extensions-digest-test.el --- Tests for Neo extension digest helpers -*- lexical-binding: t -*-

(require 'ert)
(require 'neo-extensions-digest)

(ert-deftest neo--read-binary-string-safe-round-trips-exact-bytes ()
  "The returned string's `string-bytes' must equal the file's size on disk.

Regression test: reading via `insert-file-contents-literally' into a
default (multibyte) temp buffer re-encodes any byte >= 128 using
Emacs's internal 2-byte \"raw 8-bit\" representation, so `string-bytes'
of the result silently exceeds the file's actual size. `create-image'
hands exactly `string-bytes' worth of data to the PNG decoder, so that
mismatch corrupted every embedded emblem and made it render as a
blank/broken image, even though `length' (used by an earlier ad hoc
sanity check) matched the file size and looked fine."
  (let* ((file (make-temp-file "neo-digest-test-"))
         ;; Bytes >= 128 are exactly the case that triggers multibyte
         ;; re-encoding; a real PNG is full of them.
         (raw-bytes (apply #'unibyte-string (number-sequence 0 255))))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'binary))
            (write-region raw-bytes nil file nil 0))
          (let ((result (neo--read-binary-string-safe file)))
            (should-not (multibyte-string-p result))
            (should (= (string-bytes result) (nth 7 (file-attributes file))))
            (should (equal result raw-bytes))))
      (delete-file file))))

(provide 'neo-extensions-digest-test)
;;; neo-extensions-digest-test.el ends here
