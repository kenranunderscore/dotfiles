(require 'org)

;; This checks whether there already exists an up-to-date
;; byte-compiled configuration file, otherwise it is created before
;; loading it.
;; TODO Measure startup time to see if byte-compiling is worth it
;; TODO Add a hook that does this on saving config.org
(let* ((config (concat user-emacs-directory "config.org"))
       (compiled-config (concat user-emacs-directory "config.el"))
       (byte-compiled-config (concat compiled-config "c")))
  (when (or (not (file-exists-p byte-compiled-config))
	    (file-newer-than-file-p config byte-compiled-config))
    (org-babel-tangle-file config compiled-config)
    (byte-compile-file compiled-config))
  (load-file byte-compiled-config))
