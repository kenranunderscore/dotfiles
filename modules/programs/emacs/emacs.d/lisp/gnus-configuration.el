(use-package gnus
  :config
  (setq user-full-name "Johannes Maier")
  (setq user-mail-address "johannes.maier@mailbox.org")
  (setq message-directory "~/.gnus")
  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq send-mail-function 'message-send-mail-with-sendmail)
  (setq message-sendmail-envelope-from 'header)
  (setq mail-envelope-from 'header)
  (setq mail-specify-envelope-from 'header)
  (setq gnus-gcc-mark-as-read t)
  (setq nnml-directory "~/.gnus")
  (setq gnus-interactive-exit t)
  (setq gnus-asynchronous t)
  (setq gnus-use-article-prefetch 15)
  (setq gnus-select-method '(nnnil ""))
  (setq gnus-secondary-select-methods
        '((nnimap "work"
                  (nnimap-address "imap.active-group.de")
                  (nnimap-server-port 993)
                  (nnimap-stream ssl)))))
