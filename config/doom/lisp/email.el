;; This is used when composing mails.
(setq user-full-name "Johannes Maier")
(setq my-mu4e-gmail-account-name "private")

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
  (setq mail-envelope-from 'header)

  ;; Try removing gmail's Inbox tag.
  (add-hook! 'mu4e-mark-execute-pre-hook
    (defun +mu4e-gmail-fix-flags-h (mark msg)
      ;; Only do something special for my gmail account
      (let* ((maildir (plist-get msg :maildir))
             (gmail-dir-prefix (concat "/" my-mu4e-gmail-account-name))
             (is-gmail? (string-prefix-p gmail-dir-prefix maildir)))
        (when is-gmail?
          ;; I'll leave this in, but it doesn't work with the way I do
          ;; gmail in mu4e. That is, refiling doesn't remove the actual
          ;; Inbox tag, because I target those mails from within the All
          ;; Mail directory.
          (pcase mark
            (`trash  (mu4e-action-retag-message msg "-\\Inbox,+\\Trash,-\\Draft"))
            (`refile (mu4e-action-retag-message msg "-\\Inbox"))
            (`flag   (mu4e-action-retag-message msg "+\\Starred"))
            (`unflag (mu4e-action-retag-message msg "-\\Starred"))))))))

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
                              "Geschäftsführer: Dr. Michael Sperber"))
   (mu4e-sent-messages-behavior . sent)
   (mu4e-index-cleanup . t)
   (mu4e-index-lazy-check . nil)
   (cons 'mu4e-marks my-default-mu4e-marks)))

(set-email-account!
 my-mu4e-gmail-account-name
 '((user-mail-address      . "johb.maier@gmail.com")
   (mu4e-sent-folder       . "/private/[Google Mail].All Mail")
   (mu4e-compose-signature . nil)
   (mu4e-drafts-folder     . "/private/[Google Mail].Drafts")
   (mu4e-trash-folder      . "/private/[Google Mail].Trash")
   (mu4e-refile-folder     . "/private/[Google Mail].All Mail")
   (mu4e-sent-messages-behavior . delete)
   (mu4e-index-cleanup . nil)
   (mu4e-index-lazy-check . t)
   (cons 'mu4e-marks my-gmail-mu4e-marks))
 t)
