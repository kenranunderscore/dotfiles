;;; .doom.d/config.el -*- lexical-binding: t; -*-

(cond (IS-LINUX (load! "lisp/linux.el"))
      (IS-MAC (load! "lisp/macos.el")))

(load! "lisp/email.el")

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
