;; Used by message-mode.
(setq user-full-name "Johannes Maier")

(defun my--notmuch-search-mark-read ()
  "Toggle unread tag at point in notmuch-search-mode."
  (interactive)
  (if (member "unread" (notmuch-search-get-tags))
      (notmuch-search-tag (list "-unread"))
    (notmuch-search-tag (list "+unread")))
  (notmuch-search-next-thread))

(defun my--notmuch-search-delete-mail ()
  "Toggle deleted tag at point in notmuch-search-mode."
  (interactive)
  (let ((current-tags (notmuch-search-get-tags)))
    (if (member "deleted" current-tags)
        (notmuch-search-tag (list "-deleted"))
      (notmuch-search-tag (list "+deleted" "-unread"))))
  (notmuch-search-next-thread))

(defun my--notmuch-show-delete-mail ()
  "Toggle deleted tag at point in notmuch-show-mode."
  (interactive)
  (if (member "deleted" (notmuch-show-get-tags))
      (notmuch-show-tag (list "-deleted"))
    (notmuch-show-tag (list "+deleted")))
  (unless (notmuch-show-next-open-message)
    (notmuch-show-next-thread t)))

;; I've tried and used mu4e in the past, but always liked the idea of
;; notmuch better.
(use-package notmuch
  :defer t
  :config
  (setq user-mail-address "johannes.maier@mailbox.org")
  :custom
  ;; Settings for message-mode
  (message-send-mail-function 'message-send-mail-with-sendmail)
  (message-kill-buffer-on-exit t)
  (message-sendmail-envelope-from 'header)
  ;; When replying to mail, choose the account according to the
  ;; recipient address
  (mail-envelope-from 'header)
  (mail-specify-envelope-from 'header)
  (mail-user-agent 'message-user-agent)
  ;; Notmuch-specific settings
  (notmuch-show-logo nil)
  (notmuch-show-all-multipart/alternative-parts nil)
  (notmuch-always-prompt-for-sender t)
  (notmuch-hello-sections
   '(notmuch-hello-insert-header
     notmuch-hello-insert-saved-searches
     notmuch-hello-insert-footer))
  (notmuch-search-oldest-first nil)
  (notmuch-archive-tags '("-inbox" "-unread"))
  (notmuch-saved-searches
   '((:name "inbox" :query "tag:inbox" :key "i")
     (:name "sent" :query "tag:sent" :key "s")
     (:name "work" :query "tag:inbox and tag:work" :key "w")
     (:name "private" :query "tag:inbox and tag:private" :key "p")
     (:name "all mail" :query "*" :key "a")
     (:name "lists" :query "tag:list and tag:unread" :key "l")))
  (notmuch-maildir-use-notmuch-insert t)
  (notmuch-message-replied-tags '("+replied" "+sent"))
  (notmuch-fcc-dirs
   '(("johannes.maier@active-group.de" . "ag/Sent -new +sent +work")
     ("johannes.maier@mailbox.org" . "mailbox/Sent -new +sent +private")
     (".*" . "sent")))
  :bind
  (:map notmuch-show-mode-map
        ("d" . my--notmuch-show-delete-mail)
        :map notmuch-search-mode-map
        ("d" . my--notmuch-search-delete-mail)
        ("u" . my--notmuch-search-mark-read)))

;; Enable storing links to emails in notmuch.
(use-package ol-notmuch
  :after (org notmuch)
  :commands (org-notmuch-store-link))

(with-leader
  "m" '(notmuch :which-key "mail"))

;; To switch identities (which I basically only use to set my work
;; signature based on my From address), I use gnus-alias.
(use-package gnus-alias
  :defer t
  :config
  (setq gnus-alias-identity-alist
        `(("mailbox"
           nil
           "Johannes Maier <johannes.maier@mailbox.org>"
           nil
           nil
           nil
           nil)
          ("ag"
           nil
           "Johannes Maier <johannes.maier@active-group.de>"
           "Active Group GmbH"
           nil
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
  :init
  (add-hook 'message-setup-hook 'gnus-alias-determine-identity))
