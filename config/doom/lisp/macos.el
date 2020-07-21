;;; ~/.config/doom/lisp/macos.el -*- lexical-binding: t; -*-

(setq doom-font (font-spec :family "Hack" :size 20))

;; mu4e
(setq +mu4e-backend 'offlineimap)
(mu4e-alert-set-default-style 'notifier)

(mu4e-alert-enable-notifications)
(mu4e-alert-enable-mode-line-display)

(remove-hook! mu4e-compose-mode #'org-mu4e-compose-org-mode)

(setq doom-modeline-mu4e t)
(set-email-account!
 "ag"
 '((mu4e-maildir . "/Users/maier/.mail")
   (mu4e-sent-folder . "/ag/sent")
   (mu4e-drafts-folder . "/ag/drafts")
   (mu4e-trash-folder . "/ag/trash")
   (mu4e-refile-folder . "/ag/all_mail")
   (mu4e-update-interval . 60)
   (mu4e-compose-signature . "Johannes Maier\njohannes.maier@active-group.de\n\n+49 (7071) 70896-67\n\nActive Group GmbH\nHechinger Str. 12/1\n72072 Tübingen\nRegistergericht: Amtsgericht Stuttgart, HRB 224404\nGeschäftsführer: Dr. Michael Sperber")
   (mu4e-confirm-quit . t)
   (smtpmail-smtp-user . "maier")
   (smtpmail-default-smtp-server . "smtp.active-group.de")
   (smtpmail-smtp-server . "smtp.active-group.de")
   (smtpmail-smtp-service . 587)
   (smtpmail-local-domain . "active-group.de")
   (user-mail-address . "johannes.maier@active-group.de"))
 t)
