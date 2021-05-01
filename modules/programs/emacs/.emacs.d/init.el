(require 'org)

;; This checks whether there already exists an up-to-date
;; byte-compiled configuration file, otherwise it is created before
;; loading it.
;; TODO Add a hook that does this on saving config.org
(let* ((config (concat user-emacs-directory "config.org"))
       (compiled-config (concat user-emacs-directory "config.el")))
  (when (or (not (file-exists-p compiled-config))
	    (file-newer-than-file-p config compiled-config))
    (org-babel-tangle-file config compiled-config))
  (load-file compiled-config))
