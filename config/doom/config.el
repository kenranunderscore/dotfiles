;;; .doom.d/config.el -*- lexical-binding: t; -*-

(cond (IS-LINUX (load! "lisp/linux.el"))
      (IS-MAC (load! "lisp/macos.el")))

(setq confirm-kill-emacs nil)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq doom-localleader-key ",")

(map!
 (:leader
  ;; I'm used to doing SPC SPC for M-x
  :desc "M-x" :nv "SPC" #'counsel-M-x

  ;; projectile
  (:prefix "p"
   :desc "Kill project buffers" :n "k" #'projectile-kill-buffers)

  ;; code
  (:prefix "c"
   :n "l" #'comment-line)

  ;; window
  (:prefix "w"
   :n "d" #'evil-window-delete)))

;; Prevent closing the C-c C-l popup from asking
;; for Haskell process termination
(setq-hook! 'haskell-interactive-mode-hook +popup--inhibit-transient t)

;; Elm 0.19 no longer uses "elm-package.json"
(after! elm-mode
  (setq elm-package-json "elm.json")
  (set-popup-rule! "^\\*elm-make\\*" :select nil))

;; org-mode config
(after! org
  (setq org-log-done 'time)
  (setq org-agenda-files '("~/org")))

;; lsp-mode needs another leader key due to i3
(after! lsp-mode
  (map! :leader
        (:prefix "l"
         :desc "LSP" "l" lsp-command-map)))

;; glsl-mode
(add-to-list
 'auto-mode-alist
 '("\\.\\(vs\\|vert\\|fs\\|frag\\|gs\\|geom\\|glsl\\|tesc\\|tese\\|comp\\)\\'" . glsl-mode))

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
                              "Geschäftsführer: Dr. Michael Sperber")))
 t)

(set-email-account!
 "private"
 '((mu4e-sent-folder       . "/private/[Google Mail]/Sent Mail")
   (mu4e-drafts-folder     . "/private/[Google Mail]/Drafts")
   (mu4e-trash-folder      . "/private/[Google Mail]/Trash")
   (mu4e-refile-folder     . "/private/[Google Mail]/All Mail")
   (user-mail-address      . "johb.maier@gmail.com")
   (mu4e-compose-signature . ""))
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
