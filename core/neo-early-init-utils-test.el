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
  (should (memq #'neo--defer-apply-restored-frame-geometry after-make-frame-functions))
  (should (memq #'neo/save-initial-frame-properties kill-emacs-hook))
  (should (fboundp 'neo--repair-collapsed-frame))
  (should (not (featurep 'neo-ui-frame))))

(ert-deftest neo/frame-collapse-repair-runs-exactly-once-no-retries-or-reactive-hook ()
  "The collapse repair must not retry on timers or react to size-change events.

Regression test: earlier iterations of this repair scheduled retry timers
\(0.2s-5s\) on `emacs-startup-hook' and armed a reactive
`window-size-change-functions' hook, so a single frame could be resized
several times over the first few seconds of every boot -- each a separate
visible size change during startup, independent of whether the frame was
ever actually collapsed.  The collapse this repairs never self-corrects
once it happens, so one deferred pass (from `after-make-frame-functions')
is correct and sufficient; there is nothing left to retry or react to."
  (should-not (fboundp 'neo--schedule-frame-collapse-retries))
  (should-not (memq #'neo/apply-restored-frame-geometry emacs-startup-hook))
  (should-not window-size-change-functions))

(ert-deftest neo/apply-restored-frame-geometry-does-not-resize-a-legitimately-smaller-frame ()
  "A frame correctly created smaller than the NEO default must not be resized.

Regression test: `neo/ensure-frame-onscreen-and-usable' floors any frame up
to at least the NEO default (140x42) unconditionally, which is exactly
right for its intended caller (`neo-projects.el', guaranteeing room for a
saved perspective layout) but wrong at frame-creation time -- wiring it
into `after-make-frame-functions' forced every saved geometry smaller than
the default back up to 140x42 on every single launch, which is the
\"starts wrong, then changes\" symptom.  The frame-creation path must use
only `neo--repair-collapsed-frame', which leaves a frame alone unless it is
below the true collapse threshold (`neo/minimum-frame-cols'/
`neo/minimum-frame-rows'), far below the NEO default."
  (let ((frame 'fake-frame)
        (resize-calls nil))
    (cl-letf (((symbol-function 'frame-live-p) (lambda (_) t))
              ((symbol-function 'display-graphic-p) (lambda (_) t))
              ((symbol-function 'frame-parameter) (lambda (_ _p) nil))
              ((symbol-function 'frame-width) (lambda (_) 100))
              ((symbol-function 'frame-height) (lambda (_) 30))
              ((symbol-function 'frame-monitor-workarea) (lambda (_) '(0 0 1920 1080)))
              ((symbol-function 'frame-pixel-width) (lambda (_) 1000))
              ((symbol-function 'frame-pixel-height) (lambda (_) 700))
              ((symbol-function 'frame-position) (lambda (_) '(100 . 100)))
              ((symbol-function 'set-frame-size)
               (lambda (&rest args) (push args resize-calls)))
              ((symbol-function 'set-frame-position) (lambda (&rest _) nil)))
      (neo/apply-restored-frame-geometry frame)
      (should-not resize-calls))))

(ert-deftest neo/apply-restored-frame-geometry-never-repositions ()
  "The frame-creation repair must never reposition the frame.

Regression test: `frame-position' immediately after creation is just as
unreliable as pixel size on this GTK3 build -- even deferred by a command
loop tick, it can still read a transient/incomplete value from before the
window manager finished placing the frame.  Computing an on-screen offset
from that bogus position is what pushed the frame into the bottom-right
corner even after the size-collapse case was fixed (i.e. even when
`neo--repair-collapsed-frame' correctly leaves the frame untouched).  The
window manager is trusted to place the frame at creation time, same as
position is never saved/restored across launches."
  (let ((frame 'fake-frame)
        (reposition-calls nil))
    (cl-letf (((symbol-function 'frame-live-p) (lambda (_) t))
              ((symbol-function 'display-graphic-p) (lambda (_) t))
              ((symbol-function 'frame-parameter) (lambda (_ _p) nil))
              ((symbol-function 'frame-width) (lambda (_) 100))
              ((symbol-function 'frame-height) (lambda (_) 30))
              ((symbol-function 'set-frame-size) (lambda (&rest _) nil))
              ((symbol-function 'set-frame-position)
               (lambda (&rest args) (push args reposition-calls))))
      (neo/apply-restored-frame-geometry frame)
      (should-not reposition-calls))))

(ert-deftest neo/reposition-frame-onscreen-skips-during-transient-collapse ()
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
              ((symbol-function 'frame-pixel-width) (lambda (_) 200))
              ((symbol-function 'frame-pixel-height) (lambda (_) 200))
              ((symbol-function 'frame-position) (lambda (_) '(1700 . 900)))
              ((symbol-function 'set-frame-position)
               (lambda (&rest args) (push args reposition-calls))))
      (neo/reposition-frame-onscreen frame)
      (should-not reposition-calls))))

(ert-deftest neo/emacs-version-key-combines-instance-and-major-minor-version ()
  "The version key must embed both the instance name and major.minor version.

Regression guard for the elpaca/eln-cache version-scoping feature: a stale
byte-compiled package built under one Emacs binary was silently loaded by a
newer binary reporting a different version, so builds must be segregated by
a key that actually changes across versions."
  (cl-letf (((symbol-function 'neo/get-emacs-instance-name) (lambda () "test-instance"))
            (emacs-major-version 30)
            (emacs-minor-version 2))
    (should (equal (neo/emacs-version-key) "test-instance-emacs-30.2"))))

(ert-deftest neo/emacs-version-key-changes-with-version ()
  "Two different major.minor versions must yield two different keys, so
switching Emacs binaries gets a fresh, isolated build directory."
  (cl-letf (((symbol-function 'neo/get-emacs-instance-name) (lambda () "test-instance"))
            (emacs-major-version 30)
            (emacs-minor-version 2))
    (let ((key-a (neo/emacs-version-key)))
      (cl-letf (((symbol-function 'neo/get-emacs-instance-name) (lambda () "test-instance"))
                (emacs-major-version 32)
                (emacs-minor-version 0))
        (should-not (equal key-a (neo/emacs-version-key)))))))

(ert-deftest neo/start-configuration-seeds-only-extension-manager ()
  "`Start configuration' must boot into just the extension manager.

Regression test: an earlier version of this function also seeded
`neo:dashboard' directly, so it (and the `neo:projects'/`perspective' it
requires) loaded on the very next boot too -- racing the one-shot
`neo/manager--maybe-launch-on-startup' launch and displacing the Extension
Manager with the dashboard once the idle-timer-driven post-restore hook
fired. `neo:dashboard' is pre-selected instead from inside
`neo/manager--maybe-launch-on-startup' itself (see
extensions/extensions/neo/extension-manager/neo-extension-manager.el),
which runs after bootstrap has already completed for this boot, so it only
persists the choice for the boot after this one -- no race."
  (let (set-calls restart-called)
    (cl-letf (((symbol-function 'require) (lambda (feature &rest _) feature))
              ((symbol-function 'neo/set-config)
               (lambda (key value) (push (cons key value) set-calls)))
              ((symbol-function 'restart-emacs) (lambda () (setq restart-called t))))
      (neo/start-configuration nil)
      (should (equal (cdr (assoc "enabled-extensions" set-calls))
                      "(\"neo:extension-manager\")"))
      (should restart-called))))

(provide 'neo-early-init-utils-test)
;;; neo-early-init-utils-test.el ends here
