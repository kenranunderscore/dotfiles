(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'org)
(require 'org)

(defvar +custom-package-dir
  (concat user-emacs-directory "my-packages")
  "The directory my custom (literate) Emacs packages reside in.")

(defun +determine-tangle-target (file)
  "Determine the name of the to-be-created result of tangling FILE."
  (replace-regexp-in-string "\.org$" ".el" file))

(defun +silent+timed-org-babel-tangle (file target)
  (let ((inhibit-message t)
        (message-log-max nil)
        (start-time (current-time)))
    (org-babel-tangle-file file target)
    (set-file-times target)
    (time-subtract (current-time) start-time)))

(defun +tangle-literate-config-file (file &optional only-if-newer)
  "Tangle an org file, assuming it's part of my literate configuration.
When used interactively, FILE defaults to the currently visited one."
  (interactive (list (buffer-file-name) nil))
  (let ((target (+determine-tangle-target file)))
    (if (and only-if-newer
             (not (or (not (file-exists-p target))
                      (file-newer-than-file-p file target))))
        (message "[+litconf] skipping %s: up-to-date" file)
      (progn
        (message "[+litconf] tangling %s ..." file)
        (let ((duration (+silent+timed-org-babel-tangle file target)))
          (message
           "[+litconf] created %s in %.3f seconds"
           target
           (float-time duration)))))))

(defun +tangle-all-literate-config-files (only-if-newer)
  "Tangle all literate configuration files, which includes my custom
packages in `+custom-package-dir', at once. If ONLY-IF-NEWER is non-nil,
only tangle files whose timestamp is more recent than their target's."
  (interactive (list nil))
  (+tangle-literate-config-file
   (concat user-emacs-directory "config.org")
   only-if-newer)
  (dolist (pkg (directory-files +custom-package-dir :match "\.org$"))
    (+tangle-literate-config-file pkg only-if-newer)))

;; Tangle everything if necessary
(+tangle-all-literate-config-files t)

;; Actually load the generated configuration
(load-file (concat user-emacs-directory "config.el"))
