;; This is used when composing mails.
(setq user-full-name "Johannes Maier")

(after! mu4e
  ;; I don't use org-msg-mode.
  (remove-hook! 'mu4e-compose-pre-hook #'org-msg-mode)

  ;; Make accidentally quitting mu4e harder.
  (setq mu4e-confirm-quit t)

  ;; Remove the space in front of mu4e's main buffer
  ;; name. This way we can keep it open in the background
  ;; and more easily switch to it.
  (setq mu4e-main-buffer-name "*mu4e-main*")

  ;; Ask once initially, and when composing all
  ;; should work. In any case, ask if it doesn't.
  (setq mu4e-context-policy 'ask-if-none)
  (setq mu4e-compose-context-policy 'ask-if-none)

  ;; Use msmtp to send mails instead of smtpmail.
  ;; The following options are all necessary to respect
  ;; the account chosen by mu4e.
  (setq sendmail-program "~/.nix-profile/bin/msmtp")
  (setq send-mail-function 'sendmail-send-it)
  (setq message-send-mail-function 'sendmail-send-it)
  (setq mail-specify-envelope-from t)
  (setq message-sendmail-envelope-from 'header)
  (setq mail-envelope-from 'header)

  ;; We don't need an alert, but want info about new mails
  ;; in doom's modeline at least.
  (mu4e-alert-enable-mode-line-display)
  (setq doom-modeline-mu4e t)
  (mu4e-alert-enable-notifications)
  (mu4e-alert-set-default-style 'log)

  ;; Auto-sync and update index every 5 minutes.
  (setq mu4e-update-interval 300))

(set-email-account!
 "ag"
 '((mu4e-sent-folder       . "/ag/Sent Messages")
   (mu4e-drafts-folder     . "/ag/Drafts")
   (mu4e-trash-folder      . "/ag/Deleted Messages")
   (mu4e-refile-folder     . "/ag/Archive")
   (user-mail-address      . "johannes.maier@active-group.de")
   (mu4e-compose-signature . (concat
                              "Johannes Maier\n"
                              "johannes.maier@active-group.de\n\n"
                              "+49 (7071) 70896-67\n\n"
                              "Active Group GmbH\n"
                              "Hechinger Str. 12/1\n"
                              "72072 Tübingen\n"
                              "Registergericht: Amtsgericht Stuttgart, HRB 224404\n"
                              "Geschäftsführer: Dr. Michael Sperber"))))

(set-email-account!
 "mailbox"
 '((mu4e-sent-folder       . "/mailbox/Sent")
   (mu4e-drafts-folder     . "/mailbox/Drafts")
   (mu4e-trash-folder      . "/mailbox/Trash")
   (mu4e-refile-folder     . "/mailbox/Archive")
   (user-mail-address      . "johannes.maier@mailbox.org")
   (mu4e-compose-signature . nil)))
