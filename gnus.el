(setq ;; You need to replace this email address with your own!
      user-mail-address "mrz.vtl@gmail.com"
      ;; You need to replace this key ID with your own key ID!
      mml-secure-openpgp-signers '("17D5B5FF635941EC")
      ;; This tells Gnus to get email from Gmail via IMAP.
      gnus-select-method
      '(nnimap "gmail"
               ;; It could also be imap.googlemail.com if that's your server.
               (nnimap-address "imap.gmail.com")
               (nnimap-server-port 993)
               (nnimap-stream ssl))
      ;; This tells Gnus to use the Gmail SMTP server. This
      ;; automatically leaves a copy in the Gmail Sent folder.
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      ;; Tell message mode to use SMTP.
      message-send-mail-function 'smtpmail-send-it
      ;; Gmail system labels have the prefix [Gmail], which matches
      ;; the default value of gnus-ignored-newsgroups. That's why we
      ;; redefine it.
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"
      ;; The agent seems to confuse nnimap, therefore we'll disable it.
      gnus-agent nil
      ;; We don't want local, unencrypted copies of emails we write.
      gnus-message-archive-group nil
      ;; We want to be able to read the emails we wrote.
      mml-secure-openpgp-encrypt-to-self t)

;; Attempt to encrypt all the mails we'll be sending.
(add-hook 'message-setup-hook 'mml-secure-message-encrypt)

;; Add two key bindings for your Gmail experience.
(add-hook 'gnus-summary-mode-hook 'my-gnus-summary-keys)

(when window-system
  (setq gnus-sum-thread-tree-indent "  ")
  (setq gnus-sum-thread-tree-root "") ;; "● ")
  (setq gnus-sum-thread-tree-false-root "") ;; "◯ ")
  (setq gnus-sum-thread-tree-single-indent "") ;; "◎ ")
  (setq gnus-sum-thread-tree-vertical        "│")
  (setq gnus-sum-thread-tree-leaf-with-other "├─► ")
  (setq gnus-sum-thread-tree-single-leaf     "╰─► "))
(setq gnus-summary-line-format
      (concat
       "%0{%U%R%z%}"
       "%3{│%}" "%1{%d%}" "%3{│%}" ;; date
       "  "
       "%4{%-20,20f%}"               ;; name
       "  "
       "%3{│%}"
       " "
       "%1{%B%}"
       "%s\n"))


(setq gnus-summary-display-arrow t)

;; Set sorting functions for threads and articles globally
(setq gnus-thread-sort-functions
      '((not gnus-thread-sort-by-date)
        (not gnus-thread-sort-by-number)))

(setq gnus-article-sort-functions
      '((not gnus-article-sort-by-date)))

;; Set specific sorting parameters for all groups
(setq gnus-parameters
      '((".*"
         (gnus-thread-sort-functions
          '((not gnus-thread-sort-by-date)))
         (gnus-article-sort-functions
          '((not gnus-article-sort-by-date))))))

;; Sort summary buffer by date in descending order by default
(add-hook 'gnus-summary-mode-hook
          (lambda ()
            (gnus-summary-sort-by-date)
            (gnus-summary-reverse-sort)))

(defun my-gnus-summary-keys ()
  (local-set-key "y" 'gmail-archive)
  (local-set-key "$" 'gmail-report-spam))

(defun gmail-archive ()
  "Archive the current or marked mails.
This moves them into the All Mail folder."
  (interactive)
  (gnus-summary-move-article nil "nnimap+imap.gmail.com:[Gmail]/All Mail"))

(defun gmail-report-spam ()
  "Report the current or marked mails as spam.
This moves them into the Spam folder."
  (interactive)
  (gnus-summary-move-article nil "nnimap+imap.gmail.com:[Gmail]/Spam"))

