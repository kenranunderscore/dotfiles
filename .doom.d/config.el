;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; (setq doom-font (font-spec :family "DejaVu Sans Mono" :size 15))
(setq doom-font (font-spec :family "SF Mono" :size 16))

(setq confirm-kill-emacs nil)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq doom-localleader-key ",")

(map!
 ;; text scaling
 "M-+"     (λ! (text-scale-set 0))
 "M-="     #'text-scale-increase
 "M--"     #'text-scale-decrease

 (:leader
   ;; I'm used to doing SPC SPC for M-x
   :desc "M-x" :nv "SPC" #'counsel-M-x

   ;; projectile
   (:prefix "p"
     :desc "Find file in project" :n "f" #'projectile-find-file
     :desc "Kill project buffers" :n "k" #'projectile-kill-buffers
     :n "/" nil)

   ;; buffer
   (:prefix "b"
     :desc "Kill this buffer" :nv "d" #'kill-current-buffer
     :nv "k" nil)

   ;; file
   (:prefix "f"
     :n "/" nil)

   ;; code
   (:prefix "c"
     :n "l" #'comment-line)

   ;; window
   (:prefix "w"
     :n "d" #'evil-window-delete)

   ;; git / magit
   (:prefix "g"
     :desc "Git status"     :nv "s" #'magit-status
     :desc "Git stage hunk" :n  "g" #'git-gutter:stage-hunk))

 (:after haskell-mode
   :map haskell-mode-map
   :localleader
   :n "F" #'haskell-mode-stylish-buffer))

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
