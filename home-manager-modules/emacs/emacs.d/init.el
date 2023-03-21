(require 'org)

(defun kenran/tangle-org-file (path)
  "Tangle an org file at PATH relative to the `user-emacs-directory'
to an ELisp file."
  (let* ((absolute-path (locate-user-emacs-file path))
         (target (replace-regexp-in-string "\.org$" ".el" absolute-path)))
    (when (or (not (file-exists-p target))
              (file-newer-than-file-p absolute-path target))
      (org-babel-tangle-file absolute-path target))))

;; Load my org-mode configuration.  If its timestamp is newer than
;; that of the result of tangling it into ELisp, then trigger a
;; recreation of the ELisp config.
(kenran/tangle-org-file "my-packages/my-haskell.org")
(kenran/tangle-org-file "config.org")

(load-file (locate-user-emacs-file "config.el"))
