;;; ~/.config/doom/lisp/macos.el -*- lexical-binding: t; -*-

(setq doom-font (font-spec :family "Roboto Mono" :size 13))

;; mu4e
(setq +mu4e-backend 'offlineimap)
(set-email-account!
 "ag"
 '((mu4e-sent-folder . "/ag/sent")
   (mu4e-drafts-folder . "/ag/drafts")
   (mu4e-trash-folder . "/ag/trash")
   (mu4e-refile-folder . "/ag/all_mail")
   (smtpmail-smtp-user . "maier")
   (smtpmail-default-smtp-server . "mx.active-group.de")
   (smtpmail-smtp-server . "mx.active-group.de")
   (smtpmail-smtp-service . 587)
   (user-mail-address . "johannes.maier@active-group.de")
   (mu4e-compose-signature . "Johannes Maier\n\nActive Group GmbH"))
 t)
