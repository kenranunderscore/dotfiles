;; I want my org files to have indentation corresponding to the header
;; level.
(use-package org-indent
  :diminish org-indent-mode)

;; When writing text in org-mode, auto-fill-mode should be enable to
;; automatically break overly long lines into smaller pieces when
;; typing.  We may still use M-q to re-fill paragraph when editing
;; text.  After loading org-mode, we then run our custom font setup.
(use-package org
  :hook
  ((org-mode . auto-fill-mode)
   (org-trigger . save-buffer))
  :custom
  ((org-startup-indented t)
   (org-startup-folded 'content)
   (org-directory "~/org")
   (org-log-done t)
   (org-special-ctrl-a/e t)
   ;; If this has a value greater than 0, every RET press
   ;; keeps indenting the source block further and further.
   (org-edit-src-content-indentation 0)
   (org-default-notes-file "~/org/notes.org")
   (org-agenda-files '("~/org/inbox.org"
                       "~/org/gtd.org"
                       "~/org/someday.org"))
   (org-refile-targets `(("~/org/gtd.org" :maxlevel . 3)
                         ("~/org/someday.org" :level . 1)))
   (org-capture-templates '(("t" "Todo" entry
                             (file+headline "~/org/inbox.org" "Tasks")
                             "* TODO %i%?")
                            ("n" "Note" entry
                             (file+headline "~/org/notes.org" "Notes")
                             "* %?\n%a\nNote taken on %U")))
   (org-capture-bookmark nil)
   (org-bookmark-names-plist nil)
   (org-todo-keywords '((sequence
                         "TODO(t)"
                         "WAITING(w)"
                         "|"
                         "DONE(d)"
                         "CANCELLED(c)"))))
  :config
  (advice-add 'org-refile
              :after (lambda (&rest _) (org-save-all-org-buffers))))

;; Add some globally useful Org keybindings under SPC o, like for
;; capturing, storing links etc.
(with-leader
  "o" '(:which-key "org-mode" :ignore t)
  "o a" 'org-agenda
  "o c" 'org-capture
  "o l" 'org-store-link
  "o f" 'org-cycle-agenda-files
  "o s" 'org-save-all-org-buffers)

;; The org-bullets packages enables us to use UTF-8 characters for the
;; bullet points in org headers.
(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Render unordered list bullet points as dots instead of minus/plus.
(font-lock-add-keywords
 'org-mode
 '(("^ *\\([-]\\) "
    (0 (prog1 ()
         (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; Try out this Zettelkasten approach so many people are talking
;; about.
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  ;; Didn't work for the initial setup when set in :custom.
  (setq org-roam-directory (file-truename "~/org/roam/"))
  :custom
  (org-roam-db-location (expand-file-name
                         (concat (system-name) "-roam" ".db")
                         org-roam-directory))
  :config
  (org-roam-setup))

;; For short presentations org-present looks like it is a good option.
(use-package org-present
  :hook ((org-present-mode . (lambda ()
                               (org-present-big)
                               (org-display-inline-images)
                               (org-present-hide-cursor)
                               (org-present-read-only)))
         (org-present-mode-quit . (lambda ()
                                    (org-present-small)
                                    (org-remove-inline-images)
                                    (org-present-show-cursor)
                                    (org-present-read-write)))))

