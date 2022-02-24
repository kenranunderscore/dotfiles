(deftheme kenran
  "Created 2021-09-26.")

;; Ideas:
;; - switch bold green/yellow

(require 'color)

(defmacro create-theme-colors ()
  "Expects the color variables to be bound."
  '(mapcar
    (lambda (entry)
      (list (car entry)
            `((t ,@(cdr entry)))))
    `((default (:foreground ,fg :background ,bg))
      (minibuffer-prompt (:foreground ,light-olive))
      (highlight (:foreground ,fg :background ,dark-blue))
      (region (:background ,dark-blue))
      (secondary-selection (:foreground "black" :background ,(color-darken-name "dark green" 7)))
      (vertical-border (:foreground "gray30"))
      (help-key-binding (:background "#002000" :box ,fg))
      (link (:foreground ,cyan :underline t))
      (font-lock-builtin-face (:foreground ,orange))
      (font-lock-comment-face (:foreground ,comment))
      (font-lock-constant-face (:foreground ,pink))
      (font-lock-doc-face (:slant oblique :foreground "olivedrab4"))
      (font-lock-function-name-face (:foreground ,cyan))
      (font-lock-keyword-face (:foreground ,yellow))
      (font-lock-preprocessor-face (:inherit (font-lock-constant-face)))
      (font-lock-string-face ((t (:foreground ,string))))
      (font-lock-type-face (:foreground "olivedrab3"))
      (font-lock-variable-name-face (:foreground ,sea-green))
      (font-lock-warning-face (:slant italic :foreground ,orange-red))
      (fringe (:background ,bg))
      (warning (:foreground ,orange-red :weight regular))
      (mode-line (:background "#041a04" :foreground ,fg :box ,fg))
      (mode-line-buffer-id (:weight bold))
      (mode-line-emphasis (:weight bold))
      (mode-line-inactive (:box "#555555" :background ,bg :foreground ,comment))
      (isearch (:foreground ,bg :weight semi-bold :background ,fg))
      (lazy-highlight (:foreground ,fg :background "blue3"))
      (show-paren-match (:foreground ,bg :background "cyan4"))
      (show-paren-mismatch (:foreground "red" :background ,dark-blue))

      ;; orderless
      (orderless-match-face-0 (:foreground ,orange))
      (orderless-match-face-1 (:foreground ,yellow))
      (orderless-match-face-2 (:foreground ,pink))
      (orderless-match-face-3 (:foreground ,light-olive))

      ;; outline-*, and by extension org-level-*
      (outline-1 (:weight semi-bold :foreground ,fg))
      (outline-2 (:foreground ,yellow))
      (outline-3 (:foreground ,cyan))
      (outline-4 (:foreground ,orange))
      (outline-5 (:foreground ,pink))
      (outline-6 (:foreground ,light-olive))
      (outline-7 (:foreground ,string))
      (outline-8 (:foreground ,fg))

      ;; company
      (company-tooltip (:background "gray10"))
      (company-tooltip-common (:foreground ,orange))
      (company-tooltip-selection (:background ,dark-blue :weight bold))

      ;; which-key
      (which-key-key-face (:foreground ,yellow))
      (which-key-group-description-face (:foreground ,sea-green))
      (which-key-command-description-face (:foreground ,fg))

      ;; dired and related
      (diredfl-dir-name (:foreground ,light-olive))
      (diredfl-file-name (:foreground ,fg))
      (diredfl-file-suffix (:foreground ,fg))
      (diredfl-ignored-file-name (:inherit (font-lock-comment-face)))

      ;; line numbers
      (line-number (:foreground "gray15"))
      (line-number-current-line (:foreground "dark green"))

      ;; org
      (org-todo (:foreground ,orange-red))
      (org-done (:foreground ,fg))
      (org-headline-todo (:foreground ,orange-red))
      (org-headline-done (:foreground ,comment :strike-through t))
      (org-document-title (:foreground ,cyan))
      (org-document-info (:foreground ,cyan))
      (org-verbatim (:foreground ,yellow))
      (org-code (:foreground ,light-olive))
      (org-block (:background "#121212"))
      (org-block-begin-line (:foreground ,comment))
      (org-block-end-line (:inherit 'org-block-begin-line :extend nil))
      (org-special-keyword (:foreground ,comment))

      ;; magit
      (magit-section-heading (:foreground ,orange :weight semi-bold))
      (magit-section-highlight (:background ,dark-blue))
      (magit-branch-local (:foreground ,yellow))
      (magit-branch-remote (:foreground ,cyan))
      (magit-tag (:foreground ,light-olive))
      (magit-diff-file-heading-highlight (:background ,dark-blue))
      (magit-diff-context-highlight (:background "gray15" :foreground "gray65"))
      (magit-diff-context (:foreground "gray40"))
      (magit-diff-hunk-heading (:background "gray12" :foreground "gray70"))
      (magit-diff-hunk-heading-highlight (:background "gray20" :foreground "gray80"))

      ;; manpages
      (Man-overstrike (:foreground ,cyan))

      ;; mu4e
      (mu4e-highlight-face (:weight semi-bold :foreground ,orange))

      ;; whitespace-mode
      (whitespace-space (:foreground ,whitespace-fg :background ,bg))
      (whitespace-tab (:foreground ,whitespace-fg :background ,bg))
      (whitespace-line (:foreground ,orange-red :background ,bg)))))

;; Set all the colors to their actual values.
(let ((bg "#040404")
      (fg "#0ac30a")
      (yellow "gold2")
      (cyan "cyan3")
      (string "dark khaki")
      (pink "#f474b4")
      (orange "#dc7612")
      (light-olive "olivedrab2")
      (comment "#707370")
      (dark-blue "#01018a")
      (sea-green "medium sea green")
      (orange-red "orange red")
      (whitespace-fg "#151515"))
  (apply #'custom-theme-set-faces
         (cons 'kenran (create-theme-colors))))

(provide-theme 'kenran)
