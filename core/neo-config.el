;;; neo-config.el --- Configuration database for Neo -*- lexical-binding: t -*-

(require 'sqlite)
(require 'neo-early-init-utils)

(defconst neo/config-db-filename "neo.sqlite")

(defvar neo/config-db-handle nil
  "Handle to the SQLite database.")

(defun neo/config-db-path ()
  "Return the path to the configuration database."
  (neo/config-file-path neo/config-db-filename))

(defun neo/init-config-db ()
  "Initialize the configuration database.
Creates the database and the config table if they don't exist."
  (let ((db-path (neo/config-db-path)))
    (condition-case err
        (progn
          (unless (file-exists-p (file-name-directory db-path))
            (make-directory (file-name-directory db-path) t))
          (setq neo/config-db-handle (sqlite-open db-path))
          (sqlite-execute neo/config-db-handle
                        "CREATE TABLE IF NOT EXISTS config (key TEXT PRIMARY KEY, value TEXT);"))
      (error
       (message "neo: failed to initialize configuration DB: %s" (error-message-string err))))))

(defun neo/get-config (key)
  "Retrieve the value for KEY from the configuration database.
Returns nil if the key is not found or if there's a DB error."
  (if (and neo/config-db-handle (sqlitep neo/config-db-handle))
      (condition-case err
          (let ((rows (sqlite-select neo/config-db-handle
                                     "SELECT value FROM config WHERE key = ?"
                                     (list key))))
            (if rows
                (caar rows)
              nil))
        (error
         (message "neo: error retrieving config key %s: %s" key (error-message-string err))
         nil))
    (message "neo: configuration DB not initialized")
    nil))

(defun neo/set-config (key value)
  "Set KEY to VALUE in the configuration database.
Inserts the key if it doesn't exist, or updates it if it does."
  (if (and neo/config-db-handle (sqlitep neo/config-db-handle))
      (condition-case err
          (sqlite-execute neo/config-db-handle
                        "INSERT OR REPLACE INTO config (key, value) VALUES (?, ?)"
                        (list key value))
        (error
         (message "neo: error setting config key %s: %s" key (error-message-string err))))
    (message "neo: configuration DB not initialized")))

(neo/init-config-db)

(provide 'neo-config)
