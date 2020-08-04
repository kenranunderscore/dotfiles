;; This is used when composing mails.
(setq user-full-name "Johannes Maier")

(set-email-account!
 "ag"
 '((mu4e-sent-folder       . "/ag/Sent Messages")
   (mu4e-drafts-folder     . "/ag/Drafts")
   (mu4e-trash-folder      . "/ag/trash")
   (mu4e-refile-folder     . "/ag/all")
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
 "private"
 '((user-mail-address      . "johb.maier@gmail.com")
   (mu4e-sent-folder       . "/private/[Google Mail].All Mail")
   (mu4e-compose-signature . nil)
   (mu4e-drafts-folder     . "/private/[Google Mail].Drafts")
   (mu4e-trash-folder      . "/private/[Google Mail].Trash")
   (mu4e-refile-folder     . "/private/[Google Mail].All Mail"))
 t)

(after! mu4e
  ;; Make accidentally quitting mu4e harder.
  (setq mu4e-confirm-quit t)
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
  (setq mail-envelope-from 'header))
