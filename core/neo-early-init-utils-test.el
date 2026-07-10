;;; neo-early-init-utils-test.el --- Tests for early init utilities -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'neo-early-init-utils)

(ert-deftest neo/load-file-ignores-missing-optional-file ()
  "Do not emit noise when an optional file is absent."
  (let ((messages nil)
        (missing-file (make-temp-name
                       (expand-file-name "neo-missing-" temporary-file-directory))))
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (should-not (neo/load-file missing-file t))
      (should-not messages))))

(ert-deftest neo/load-file-reports-optional-load-errors ()
  "Report actual load errors even when failure is allowed."
  (let ((messages nil)
        (bad-file
         (make-temp-file "neo-bad-load-" nil ".el" "(this is not valid elisp)")))
    (unwind-protect
        (cl-letf (((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (push (apply #'format format-string args) messages))))
          (should-not (neo/load-file bad-file t))
          (should (= (length messages) 1))
          (should (string-match-p
                   (regexp-quote (format "neo: error loading %s:" bad-file))
                   (car messages))))
      (delete-file bad-file))))

(ert-deftest neo/disable-customize-persistence-sets-custom-file ()
  "Point `custom-file' at `null-device' in Neo."
  (let ((custom-file "custom.el"))
    (neo/disable-customize-persistence)
    (should (equal custom-file null-device))))

(ert-deftest neo/disable-customize-persistence-blocks-custom-save-all ()
  "Make `custom-save-all' a harmless no-op in batch code paths."
  (let ((messages nil))
    (unwind-protect
        (progn
          (neo/disable-customize-persistence)
          (require 'cus-edit)
          (should (advice-member-p #'neo--custom-save-all-disabled 'custom-save-all))
          (cl-letf (((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (push (apply #'format format-string args) messages))))
            (should-not (custom-save-all))
            (should (equal messages (list neo--customize-disabled-message)))))
      (when (advice-member-p #'neo--custom-save-all-disabled 'custom-save-all)
        (advice-remove 'custom-save-all #'neo--custom-save-all-disabled)))))

(ert-deftest neo/frame-collapse-repair-hooks-install-from-core-alone ()
  "The collapsed-frame repair must be active without loading `neo-ui-frame'.

Regression test: this repair used to live only in the neo:ui extension, so
boots that never load it -- the first-run splash screen and the
extension-manager-only restart from `neo/start-configuration' -- never
installed the repair hooks, letting a collapsed ~200x200px frame go
unrepaired.  `neo-early-init-utils' is required unconditionally by
early-init.el, so its hooks must be present as soon as this file loads,
with no dependency on neo:ui."
  (should (memq #'neo/apply-restored-frame-geometry emacs-startup-hook))
  (should (memq #'neo--defer-ensure-frame-onscreen-and-usable after-make-frame-functions))
  (should (memq #'neo/save-initial-frame-properties kill-emacs-hook))
  (should (fboundp 'neo--repair-collapsed-frame))
  (should (not (featurep 'neo-ui-frame))))

(ert-deftest neo/frame-collapse-repair-not-armed-before-startup-completes ()
  "The reactive size-change hook must not be armed at file load time.

Regression test: arming `window-size-change-functions' unconditionally at
load time (i.e. from `early-init.el', before the initial frame is even
realized) let it react to the toolkit's own noisy intermediate sizes while
the window manager was still mapping the very first frame, visibly yanking
it mid-realization -- observed as the splash frame flashing and jumping to a
collapsed size/position instead of showing normally."
  (should-not (memq #'neo--repair-collapsed-frame window-size-change-functions)))

(ert-deftest neo/frame-collapse-repair-reacts-to-size-change-events-after-startup ()
  "The collapse repair must not rely solely on fixed-delay retry timers.

Regression test: the retry timers are scheduled from `early-init.el' load
time, well before the initial frame exists, so a toolkit collapse that
manifests later than the last retry would go unrepaired forever. A reactive
`window-size-change-functions' hook, armed once `neo/apply-restored-frame-geometry'
has run at least once (i.e. after Emacs's own startup has completed and the
initial frame has already been realized), catches a later collapse whenever
Emacs actually notices the frame's size changed, independent of timing."
  (let ((window-size-change-functions nil))
    (neo/apply-restored-frame-geometry)
    (should (memq #'neo--repair-collapsed-frame window-size-change-functions))))

(ert-deftest neo/ensure-frame-onscreen-and-usable-skips-reposition-during-transient-collapse ()
  "Do not reposition using pixel geometry that looks like the GTK3 collapse.

Regression test: `frame-pixel-width'/`frame-pixel-height' read right after
frame creation can transiently report the toolkit's known ~200x200px
collapse signature before the real layout settles.  Computing an on-screen
offset from those bogus numbers is what drove the frame into the
bottom-right corner on the splash boot once this repair started running
via `after-make-frame-functions' on every boot instead of only when
neo:ui happened to be loaded.  While the frame still looks collapsed,
repositioning must be skipped entirely (the resize path already brings it
up to a safe size; a later call will see real numbers and reposition then
if still needed)."
  (let ((frame 'fake-frame)
        (reposition-calls nil))
    (cl-letf (((symbol-function 'frame-live-p) (lambda (_) t))
              ((symbol-function 'display-graphic-p) (lambda (_) t))
              ((symbol-function 'frame-parameter) (lambda (_ _p) nil))
              ((symbol-function 'frame-monitor-workarea) (lambda (_) '(0 0 1920 1080)))
              ((symbol-function 'frame-char-width) (lambda (_) 10))
              ((symbol-function 'frame-char-height) (lambda (_) 20))
              ((symbol-function 'frame-width) (lambda (_) 140))
              ((symbol-function 'frame-height) (lambda (_) 42))
              ((symbol-function 'frame-pixel-width) (lambda (_) 200))
              ((symbol-function 'frame-pixel-height) (lambda (_) 200))
              ((symbol-function 'frame-position) (lambda (_) '(1700 . 900)))
              ((symbol-function 'set-frame-size) (lambda (&rest _) nil))
              ((symbol-function 'set-frame-position)
               (lambda (&rest args) (push args reposition-calls))))
      (neo/ensure-frame-onscreen-and-usable frame)
      (should-not reposition-calls))))

(provide 'neo-early-init-utils-test)
;;; neo-early-init-utils-test.el ends here
