;; Used by message-mode.
(setq user-full-name "Johannes Maier")

(require 'mu4e)

(setq mail-user-agent 'mu4e-user-agent)
(setq mu4e-completing-read-function #'completing-read)
;; I don't sync drafts to either of the accounts
(setq mu4e-drafts-folder "/drafts")
(setq mu4e-contexts
      `(,(make-mu4e-context
          :name "mailbox"
          :match-func (lambda (msg)
                        (when msg
                          t))
          :vars '((user-mail-address . "johannes.maier@mailbox.org")
                  (mu4e-compose-signature . nil)
                  (mu4e-sent-folder . "/mailbox/Sent")
                  (mu4e-trash-folder . "/mailbox/Trash")
                  (mu4e-refile-folder . "/mailbox/Archive")))
        ,(make-mu4e-context
          :name "ag"
          :match-func (lambda (msg) t)
          :vars '((user-mail-address . "johannes.maier@active-group.de")
                  (mu4e-compose-signature . ,(concat
                                              "Johannes Maier\n"
                                              "johannes.maier@active-group.de\n\n"
                                              "+49 (7071) 70896-67\n\n"
                                              "Active Group GmbH\n"
                                              "Hechinger Str. 12/1\n"
                                              "72072 Tübingen\n"
                                              "Registergericht: Amtsgericht Stuttgart, HRB 224404\n"
                                              "Geschäftsführer: Dr. Michael Sperber"))
                  (mu4e-sent-folder . "/ag/Sent")
                  (mu4e-refile-folder . "/ag/Archive")
                  (mu4e-trash-folder . "/ag/Trash")))))
(setq mu4e-context-policy 'pick-first)
(setq mu4e-compose-policy 'ask)

;; Composing emails
(setq message-send-mail-function #'message-send-mail-with-sendmail)
(setq send-mail-function #'message-send-mail-with-sendmail)
(setq message-sendmail-envelope-from 'header)
(setq mail-envelope-from 'header)
(setq mail-specify-envelope-from 'header)
(setq message-kill-buffer-on-exit t)

(with-leader
  "m" '(mu4e :which-key "mail"))
