(require 'org)

(defun +tangle-org-file (path)
  "Tangle an org file at PATH (absolute) to an ELisp file.  Will
only tangle if no target file exists yet, or if the source file
has been touched more recently than the target."
  (let* ((target (replace-regexp-in-string "\.org$" ".el" path)))
    (when (or (not (file-exists-p target))
              (file-newer-than-file-p path target))
      (message (concat "Tangling config file: "
                       (file-name-nondirectory path)))
      (org-babel-tangle-file path target)
      (set-file-times target))))

;; Tangle all my custom packages.
(setq +custom-package-dir
      (concat user-emacs-directory "my-packages"))
(dolist (f (directory-files +custom-package-dir
                            :match "\.org$"))
  (+tangle-org-file f))

(+tangle-org-file (concat user-emacs-directory "config.org"))
(load-file (concat user-emacs-directory "config.el"))
