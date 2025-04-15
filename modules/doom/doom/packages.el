;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;;; Color themes

(package! emacs-naga-theme
  :recipe (:host github
           :repo "kenranunderscore/emacs-naga-theme"
           :files ("naga-theme.el" "naga-dimmed-theme.el" "naga-base.el")))
(package! modus-themes)
(package! gruber-darker-theme)
(package! srcery-theme)
(package! base16-theme)
(package! spacemacs-theme)
(package! srcery-theme)

;;; Org mode

(package! org-appear)
(package! org-modern)
(package! org-present)

;;; E-mail

(package! gnus-alias)

;;; Keybindings

(package! hydra)

;;;  Disable default snippets
(package! doom-snippets :ignore t)

;;; Formatter definitions the right way
(package! reformatter)
