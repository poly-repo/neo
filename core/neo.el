;;;###autoload
(defvar neo/use-extensions nil
  "When t we use neo extension mechanism. nil make immediate use of use-package")


(defun neo--require (feature)
  (message "Requiring %s" feature)
  (require feature))

(neo--require 'neo-custom)			; customization groups
(neo--require 'neo-utils)			; utility functions
(neo--require 'neo-config)
(neo--require 'neo-packages)

(neo--require 'neo-struct)

(setq neo (neo/get-instance))

(defvar neo/my-enabled-extensions
  '("neo:questionable-defaults"
    "neo:modeline"
    "neo:ui"
    "neo:session"
    "neo:org"
    "neo:terminal"
    "neo:versions"
    "neo:build"
;;    "neo:lsp"
;;    "neo:python"
    "neo:leetcode"
;;    "neo:ai"
    )
  "Temporary list of extensions to load.")

(neo/install-extensions-from-slugs neo neo/my-enabled-extensions)
(message "neo--enabled-packages: %s" neo--enabled-packages)
(neo/load-installed-extensions neo)
(message "neo--enabled-packages: %s" neo--enabled-packages)
(when neo/use-extensions
  (neo/replay-installed-extensions-packages neo)
  (message "neo--enabled-packages: %s" neo--enabled-packages))
(neo/debug-info)

(provide 'neo)
