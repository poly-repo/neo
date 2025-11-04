;;;###autoload
(defvar neo/use-extensions  nil
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
    "neo:compsel"
    "neo:session"
    "neo:org"
    "neo:news"
    "neo:terminal"
    "neo:versions"
    "neo:build"
    "neo:lsp"
    "neo:python"
    "neo:c++"
    "neo:elisp"
    "neo:leetcode"
    "neo:ai"
    "neo:writer"
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
