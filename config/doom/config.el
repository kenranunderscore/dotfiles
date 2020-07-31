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

(setq notmuch-message-headers-visible t)

(autoload 'gnus-alias-determine-identity "gnus-alias" "" t)
(setq gnus-alias-identity-alist
      '(("home"
         nil
         "Johannes Maier <johb.maier@gmail.com>"
         nil
         nil
         nil
         nil)
        ("work"
         nil
         "Johannes Maier <johannes.maier@active-group.de>"
         "Active Group GmbH"
         nil
         nil
         "Johannes Maier\njohannes.maier@active-group.de\n\n+49 (7071) 70896-67\n\nActive Group GmbH\nHechinger Str. 12/1\n72072 Tübingen\nRegistergericht: Amtsgericht Stuttgart, HRB 224404\nGeschäftsführer: Dr. Michael Sperber")))
(setq gnus-alias-default-identity "home")
(setq gnus-alias-identity-rules
      '(("@active-group.de" ("any" "@active-group\\.de" both) "work")
        ("@gmail.com" ("any" "@gmail\\.com" both) "home")))
(add-hook 'message-setup-hook 'gnus-alias-select-identity)

(setq send-mail-function 'sendmail-send-it
      sendmail-program "~/.nix-profile/bin/msmtp"
      mail-specify-envelope-from t
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header)
