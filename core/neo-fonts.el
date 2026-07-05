;; -*- lexical-binding: t; -*-

(require 'neo-processes)
(require 'neo-logging)

;; TODO Discover all fonts needed by all installed extensions
(defvar neo/fonts '(
		    "Train One:google"
		    "Poiret One:google"
		    "Tourney:google"
		    "Sirivennela:google"
		    "Alan Sans:google"
		    "Roboto:google"
		    "Orbitron:google"
		    "ZedMono:nerd"
		    "Iosevka:nerd"
		    ))

(defvar neo/font-fetcher
  (seq-find #'file-exists-p
	    `(
	      ,(expand-file-name "scripts/font_fetcher/o-font-fetcher" user-emacs-directory)
	      ,(expand-file-name "../../../infra/tools/font_fetcher/o-font-fetcher" user-emacs-directory)))
  "Absolute path to the font-fetcher script, or nil when none is bundled.")

(defun neo/fetch-fonts ()
  "Fetch NEO's fonts, if the fetcher script is available.
No-op (with a logged warning) when the script is missing, so a missing
script never crashes startup.  Runs asynchronously and never blocks the UI;
the fetcher itself skips font families that are already installed.  Results
go to the NEO log (`neo/log-show'), not the echo area."
  (if (not neo/font-fetcher)
      (neo/log-warn 'fonts "Font fetcher script not found; skipping font install")
    (neo/log-info 'fonts "Fetching fonts in the background")
    (neo/execute-with-venv neo/font-fetcher
			   :args (cons (expand-file-name "~/.local/share/fonts") neo/fonts)
			   :show-buffer nil
			   :on-success (lambda () (neo/log-info 'fonts "Fonts up to date"))
			   :on-error (lambda () (neo/log-warn 'fonts "Font fetch reported errors")))))

;; Defer off the boot thread: after Elpaca finishes init, or on the next idle
;; if init already completed (e.g. this file loaded interactively).
(if after-init-time
    (run-with-idle-timer 1 nil #'neo/fetch-fonts)
  (add-hook 'elpaca-after-init-hook #'neo/fetch-fonts))

(provide 'neo-fonts)
