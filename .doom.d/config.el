;;; .doom.d/config.el -*- lexical-binding: t; -*-

;;(setq doom-font (font-spec :family "Source Code Pro" :size 14))
;; (setq doom-theme 'base16-greenscreen)

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

;; org-mode
(setq org-default-notes-file "todos.org")
;;(setq org-agenda-files '("~/org"))

;; Elm 0.19 no longer uses "elm-package.json"
(setq elm-package-json "elm.json")
(set-popup-rule! "^\\*elm-make\\*" :select nil)
