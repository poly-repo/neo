;; -*- lexical-binding: t; -*-

(require 'neo-processes)

;; TODO Discover all fonts needed by all installed extensions
(defvar neo/fonts '(
		    "Roboto:google"
		    "Orbitron:google"
		    "ZedMono:nerd"
		    "Iosevka:nerd"
		    ))

(defvar neo/font-fetcher
  (seq-find #'file-exists-p
	    `(
	      ,(expand-file-name "scripts/font_fetcher/o-font-fetcher" user-emacs-directory)
	      ,(expand-file-name "../../../infra/tools/font_fetcher/o-font-fetcher" user-emacs-directory))))

(neo/execute neo/font-fetcher :args (cons "/tmp/fonts" neo/fonts)
	     :show-buffer nil
	     :on-success (lambda () (message "Success"))
	     :on-error (lambda () (message "Error")))

(provide 'neo-fonts)
