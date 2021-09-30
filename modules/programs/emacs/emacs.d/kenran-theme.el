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
      (highlight (:foreground ,fg :background "gray12" :weight semi-bold))
      (region (:background "blue4"))
      (font-lock-builtin-face (:foreground ,orange))
      (font-lock-comment-face (:slant italic :foreground ,comment))
      (font-lock-constant-face (:foreground ,pink))
      (font-lock-doc-face (:slant italic :foreground "olivedrab4"))
      (font-lock-function-name-face (:foreground ,cyan))
      (font-lock-keyword-face (:foreground ,yellow))
      (font-lock-preprocessor-face (:inherit (font-lock-constant-face)))
      (font-lock-string-face ((t (:foreground ,string))))
      (font-lock-type-face (:foreground "olivedrab3"))
      (font-lock-variable-name-face (:foreground ,fg))
      (font-lock-warning-face (:slant italic :foreground "orange red"))
      (fringe (:background ,bg))
      (warning (:foreground "orange red" :weight regular))
      (mode-line (:background ,bg :foreground ,light-olive :box "#cccccc"))
      (mode-line-buffer-id (:weight bold))
      (mode-line-emphasis (:weight bold))
      (mode-line-inactive (:box "#555555" :background ,bg :foreground ,comment))
      (isearch (:foreground ,bg :weight semi-bold :background ,fg))
      (lazy-highlight (:foreground ,fg :background "blue3"))
      (show-paren-match (:foreground ,bg :background ,fg))

      ;; orderless
      (orderless-match-face-0 (:foreground ,light-olive))
      (orderless-match-face-1 (:foreground ,cyan))
      (orderless-match-face-2 (:foreground ,orange))
      (orderless-match-face-3 (:foreground ,yellow))

      ;; outline-*, and by extension org-level-*
      (outline-1 (:weight semi-bold :foreground ,fg))
      (outline-2 (:foreground ,yellow))
      (outline-3 (:foreground ,cyan))
      (outline-4 (:foreground ,orange))
      (outline-5 (:foreground ,pink))
      (outline-6 (:foreground "olivedrab2"))
      (outline-7 (:foreground ,string))
      (outline-8 (:foreground ,fg))

      ;; company
      (company-tooltip (:background "gray10"))
      (company-tooltip-common (:foreground ,light-olive))
      (company-tooltip-selection (:background "blue4" :weight bold))

      ;; which-key
      (which-key-key-face (:foreground ,yellow))
      (which-key-group-description-face (:foreground ,light-olive))
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
      (org-todo (:foreground "orange red"))
      (org-done (:foreground ,fg))
      (org-headline-todo (:foreground "orange red"))
      (org-headline-done (:foreground ,comment :strike-through t)))))

;; Set all the colors to their actual values.
(let ((bg "#040404")
      (fg "green3")
      (yellow "yellow3")
      (cyan "cyan3")
      (string "dark khaki")
      (pink "hot pink")
      (orange "#dc7612")
      (light-olive "olivedrab2")
      (comment "#707370"))
  (apply #'custom-theme-set-faces
         (cons 'kenran (create-theme-colors))))

(provide-theme 'kenran)
