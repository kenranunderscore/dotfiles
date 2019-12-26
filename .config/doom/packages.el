;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

;; Use a fork of emacs-doom-themes to be able to develop
;; custom themes myself
(package! emacs-doom-themes
  :recipe (:host github :repo "kenranunderscore/emacs-doom-themes" :branch "master"))

(package! glsl-mode)
(package! mu4e-alert)
