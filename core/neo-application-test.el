;;; neo-application-test.el --- Test for Neo applications -*- lexical-binding: t -*-

(require 'neo-application)

;; Define a Calc application
(neo/application "Calc"
  :setup (calc)
  :bind "c")

;; To test:
;; 1. Load this file.
;; 2. The command `neo/app-calc` should be defined.
;; 3. `(gethash "Calc" neo--applications)` should return the application struct.
;; 4. If `neo/applications-map` is bound to a prefix (e.g., C-x a), then `C-x a c` would run calc.

(provide 'neo-application-test)
