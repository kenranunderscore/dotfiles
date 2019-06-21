;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(setq doom-font (font-spec :family "Input" :size 15))
(setq doom-theme 'doom-one)

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
   :desc "M-x" :nv "SPC" #'helm-M-x

   ;; projectile
   (:desc "project" :prefix "p"
     :desc "Find file in project" :n "f" #'projectile-find-file
     :desc "Kill project buffers" :n "k" #'projectile-kill-buffers
     :n "/" nil)

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
   (:desc "git" :prefix "g"
     :desc "Git status"     :nv "s" #'magit-status
     :desc "Git stage hunk" :n  "g" #'git-gutter:stage-hunk))

 (:after haskell-mode
   :map haskell-mode-map
   :localleader
   :n "F" #'haskell-mode-stylish-buffer))
