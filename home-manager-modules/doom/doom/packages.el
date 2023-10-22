;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;;; Color themes

(package! emacs-naga-theme
  :recipe (:host github :repo "kenranunderscore/emacs-naga-theme"
           :files ("naga-theme.el")))
(package! modus-themes)
(package! gruber-darker-theme)
(package! srcery-theme)

;;; Org mode

(package! org-appear)
(package! org-modern)

;;; E-mail

(package! mu4e-alert :disable t)

;;; Keybindings

(package! hydra)
