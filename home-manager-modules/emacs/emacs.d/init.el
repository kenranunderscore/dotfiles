(require 'org)

;; Load my org-mode configuration.  If its timestamp is newer than
;; that of the result of tangling it into ELisp, then trigger a
;; recreation of the ELisp config.
(let* ((config-org (concat user-emacs-directory "config.org"))
       (config-el (concat user-emacs-directory "config.el")))
  (when (or (not (file-exists-p config-el))
            (file-newer-than-file-p config-org config-el))
    (org-babel-tangle-file config-org config-el))
  (load-file config-el))
