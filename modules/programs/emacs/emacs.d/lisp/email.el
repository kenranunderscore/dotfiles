;; Used by message-mode.
(setq user-full-name "Johannes Maier")

(use-package mu4e
  :ensure nil
  :defer t
  :commands (mu4e)
  :config
  (setq mail-user-agent 'mu4e-user-agent)
  (setq mu4e-completing-read-function #'completing-read)
  ;; I don't sync drafts to either of the accounts
  (setq mu4e-drafts-folder "/drafts")
  (setq mu4e-contexts
        `(,(let ((addr "johannes.maier@mailbox.org"))
             (make-mu4e-context
              :name "mailbox"
              :match-func (lambda (msg)
                            (when msg
                              (mu4e-message-contact-field-matches msg (:to :from :cc :bcc) addr)))
              :vars `((user-mail-address . ,addr)
                      (mu4e-compose-signature . nil)
                      (mu4e-sent-folder . "/mailbox/Sent")
                      (mu4e-trash-folder . "/mailbox/Trash")
                      (mu4e-refile-folder . "/mailbox/Archive"))))
          ,(let ((addr "johannes.maier@active-group.de"))
             (make-mu4e-context
              :name "ag"
              :match-func (lambda (msg)
                            (when msg
                              (mu4e-message-contact-field-matches msg (:to :from :cc :bcc) addr)))
              :vars `((user-mail-address . ,addr)
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
                      (mu4e-trash-folder . "/ag/Trash"))))))
  (setq mu4e-context-policy 'pick-first)
  (setq mu4e-compose-policy 'ask)

  ;; Getting mail via mbsync
  (setq mu4e-get-mail-command "mbsync -a")

  ;; Composing emails
  (setq message-send-mail-function #'message-send-mail-with-sendmail)
  (setq send-mail-function #'message-send-mail-with-sendmail)
  (setq message-sendmail-envelope-from 'header)
  (setq mail-envelope-from 'header)
  (setq mail-specify-envelope-from 'header)
  (setq message-kill-buffer-on-exit t)

  ;; Visuals
  (setq mu4e-headers-thread-single-orphan-prefix '("─> " . "─▶"))
  (setq mu4e-headers-thread-orphan-prefix '("┬> " . "┬▶ "))
  (setq mu4e-headers-thread-child-prefix '("├> " . "├▶"))
  (setq mu4e-headers-thread-connection-prefix '("│ " . "│ "))
  (setq mu4e-headers-thread-duplicate-prefix '("= " . "≡ "))
  (setq mu4e-headers-thread-first-child-prefix '("├> " . "├▶"))
  (setq mu4e-headers-thread-last-child-prefix '("└> " . "╰▶")))

(with-leader
  "m" '(mu4e :which-key "mail"))
