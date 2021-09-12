(use-package gnus
  :init
  (setq gnus-home-directory "~/sync")
  :config
  (setq user-full-name "Johannes Maier")
  (setq user-mail-address "johannes.maier@mailbox.org")
  (setq message-directory "~/.gnus")
  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq send-mail-function 'message-send-mail-with-sendmail)
  (setq message-sendmail-envelope-from 'header)
  (setq mail-envelope-from 'header)
  (setq mail-specify-envelope-from 'header)
  (setq gnus-check-new-newsgroups nil)
  (setq gnus-gcc-mark-as-read t)
  (setq nnml-directory "~/.gnus")
  (setq gnus-interactive-exit t)
  (setq gnus-asynchronous t)
  (setq gnus-use-article-prefetch 15)
  (setq gnus-select-method '(nnnil ""))
  (setq gnus-secondary-select-methods
        '((nntp "news.gwene.org")
          (nnimap "ag"
                  (nnimap-address "imap.active-group.de")
                  (nnimap-server-port 993)
                  (nnimap-stream ssl))
          (nnimap "mailbox"
                  (nnimap-address "imap.mailbox.org")
                  (nnimap-server-port 993)
                  (nnimap-stream ssl)))))

;; FIXME gcc: should it put sent mail into the group it came from?
;; Is that perhaps the default?
;; I would like to:
;;  a) see threads
;;  b) have sent mail available on the server

;; To switch identities (which I basically only use to set my work
;; signature based on my From address), I use gnus-alias.
(use-package gnus-alias
  :after gnus
  :config
  (setq gnus-alias-identity-alist
        `(("mailbox"
           nil
           "Johannes Maier <johannes.maier@mailbox.org>"
           nil
           (("Gcc" . "nnimap+mailbox:Sent"))
           nil
           nil)
          ("ag"
           nil
           "Johannes Maier <johannes.maier@active-group.de>"
           "Active Group GmbH"
           (("Gcc" . "nnimap+ag:Sent"))
           nil
           ,(concat
             "Johannes Maier\n"
             "johannes.maier@active-group.de\n\n"
             "+49 (7071) 70896-67\n\n"
             "Active Group GmbH\n"
             "Hechinger Str. 12/1\n"
             "72072 Tübingen\n"
             "Registergericht: Amtsgericht Stuttgart, HRB 224404\n"
             "Geschäftsführer: Dr. Michael Sperber"))))
  (setq gnus-alias-default-identity "mailbox")
  (setq gnus-alias-identity-rules
        '(("ag" ("any" "@active-group.de" both) "ag")))
  :init (add-hook 'message-setup-hook #'gnus-alias-determine-identity))

(with-local-leader
  :states 'normal
  :keymaps 'message-mode-map
  "i" '(gnus-alias-select-identity :which-key "select identity"))
