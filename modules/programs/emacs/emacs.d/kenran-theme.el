(deftheme kenran
  "Created 2021-09-26.")

(defmacro create-theme-colors ()
  "Expects the color variables to be bound."
  '(mapcar
    (lambda (entry)
      (list (car entry)
            `((t ,@(cdr entry)))))
    `((default (:foreground ,fg :background ,bg))
      (minibuffer-prompt (:foreground "olivedrab3"))
      (highlight (:foreground ,fg :background "gray12" :weight semi-bold))
      (region (:background "blue4"))
      (font-lock-builtin-face (:foreground "#dc7612"))
      (font-lock-comment-face (:slant italic :foreground "#707370"))
      (font-lock-constant-face (:foreground "hot pink"))
      (font-lock-doc-face (:slant italic :foreground "olivedrab4"))
      (font-lock-function-name-face (:foreground "cyan2"))
      (font-lock-keyword-face (:foreground "green3" :weight bold))
      (font-lock-preprocessor-face (:inherit (font-lock-constant-face)))
      (font-lock-string-face ((t (:foreground "dark khaki"))))
      (font-lock-type-face (:foreground "yellow2"))
      (font-lock-variable-name-face (:foreground ,fg))
      (fringe (:background ,bg))
      (warning (:foreground "orange red" :weight regular))
      (mode-line (:background ,bg :foreground "olivedrab3" :box "#cccccc"))
      (mode-line-buffer-id (:weight bold))
      (mode-line-emphasis (:weight bold))
      (mode-line-inactive (:box "#555555" :background ,bg :foreground "#707370"))
      (isearch (:foreground ,bg :weight semi-bold :background ,fg))
      (lazy-highlight (:foreground ,fg :background "blue3"))
      (show-paren-match (:foreground ,bg :background ,fg))

      ;; orderless
      (orderless-match-face-0 (:foreground "yellow"))
      (orderless-match-face-1 (:foreground "hot pink"))
      (orderless-match-face-2 (:foreground "dark khaki"))
      (orderless-match-face-3 (:foreground "cyan2")))))

;; Set all the colors to their actual values.
(let ((bg "#040404")
      (fg "green3"))
  (apply #'custom-theme-set-faces
         (cons 'kenran (create-theme-colors))))

(provide-theme 'kenran)
