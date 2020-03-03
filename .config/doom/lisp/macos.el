;;; ~/.config/doom/lisp/macos.el -*- lexical-binding: t; -*-

(setq doom-font (font-spec :family "SF Mono" :size 17))

;; mu4e
(setq +mu4e-backend 'offlineimap)
(mu4e-alert-set-default-style 'notifier)
(add-hook! mu4e-main-mode #'mu4e-alert-enable-notifications)
(add-hook! mu4e-main-mode #'mu4e-alert-enable-mode-line-display)
(setq doom-modeline-mu4e t)
(set-email-account!
 "ag"
 '((mu4e-maildir . "/Users/maier/.mail")
   (mu4e-sent-folder . "/ag/sent")
   (mu4e-drafts-folder . "/ag/drafts")
   (mu4e-trash-folder . "/ag/trash")
   (mu4e-refile-folder . "/ag/all_mail")
   (mu4e-update-interval . 60)
   (smtpmail-smtp-user . "maier")
   (smtpmail-default-smtp-server . "mx.active-group.de")
   (smtpmail-smtp-server . "mx.active-group.de")
   (smtpmail-smtp-service . 587)
   ;; (smtpmail-local-domain . "active-group.de")
   (user-mail-address . "johannes.maier@active-group.de")
   (mu4e-compose-signature . "Johannes Maier\n\nActive Group GmbH"))
 t)
