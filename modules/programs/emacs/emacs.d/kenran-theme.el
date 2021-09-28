(deftheme kenran
  "Created 2021-09-26.")

(let ((bg "#040404")
      (fg "green3"))
  (apply #'custom-theme-set-faces
         `(kenran
           (default ((t (:foreground ,fg :background ,bg))))
           (minibuffer-prompt ((t (:foreground "olivedrab3"))))
           (highlight ((t (:foreground ,fg :background "gray12" :weight semi-bold))))
           (region ((t (:background "blue4"))))
           (font-lock-builtin-face ((t (:foreground "#dc7612"))))
           (font-lock-comment-face ((t (:slant italic :foreground "#707370"))))
           (font-lock-constant-face ((t (:foreground "hot pink"))))
           (font-lock-doc-face ((t (:slant italic :foreground "olivedrab4"))))
           (font-lock-function-name-face ((t (:foreground "cyan2"))))
           (font-lock-keyword-face ((t (:foreground "green3" :weight bold))))
           (font-lock-preprocessor-face ((t (:inherit (font-lock-constant-face)))))
           (font-lock-string-face ((t (:foreground "dark khaki"))))
           (font-lock-type-face ((t (:foreground "yellow2"))))
           (font-lock-variable-name-face ((t (:foreground ,fg))))
           (fringe ((t (:background ,bg))))
           (warning ((t (:foreground "orange red" :weight regular))))
           (mode-line ((t (:background ,bg :foreground "olivedrab3" :box "#cccccc"))))
           (mode-line-buffer-id ((t (:weight bold))))
           (mode-line-emphasis ((t (:weight bold))))
           (mode-line-inactive ((t (:box "#555555" :background ,bg :foreground "#707370"))))
           (isearch ((t (:foreground ,bg :weight semi-bold :background ,fg))))
           (lazy-highlight ((t (:foreground ,fg :background "blue3"))))
           (show-paren-match ((t (:foreground ,bg :background ,fg))))

           ;; orderless
           (orderless-match-face-0 ((t (:foreground "yellow"))))
           (orderless-match-face-1 ((t (:foreground "hot pink"))))
           (orderless-match-face-2 ((t (:foreground "dark khaki"))))
           (orderless-match-face-3 ((t (:foreground "cyan2")))))))

(provide-theme 'kenran)
