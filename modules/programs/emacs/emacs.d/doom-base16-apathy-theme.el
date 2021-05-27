;;; doom-base16-apathy-theme.el --- inspired by base16-apathy -*- no-byte-compile: t; -*-
(require 'doom-themes)

;;
(defgroup doom-base16-apathy-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-base16-apathy-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-base16-apathy-theme
  :type 'boolean)

(defcustom doom-base16-apathy-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-base16-apathy-theme
  :type 'boolean)

(defcustom doom-base16-apathy-comment-bg doom-base16-apathy-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-base16-apathy-theme
  :type 'boolean)

(defcustom doom-base16-apathy-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-base16-apathy-theme
  :type '(choice integer boolean))

;;
(def-doom-theme doom-base16-apathy
  "A dark theme inspired by base16-apathy"

  ;; name        default   256       16
  ((bg         '("#031a16" nil       nil            ))
   (bg-alt     '("#031a16" nil       nil            ))
   (base0      '("#031a16" "black"   "black"        ))
   (base1      '("#0b342d" "#1e1e1e" "brightblack"  ))
   (base2      '("#184e45" "#2e2e2e" "brightblack"  ))
   (base3      '("#2b685e" "#262626" "brightblack"  ))
   (base4      '("#5f9c92" "#3f3f3f" "brightblack"  ))
   (base5      '("#81b5ac" "#525252" "brightblack"  ))
   (base6      '("#a7cec8" "#6b6b6b" "brightblack"  ))
   (base7      '("#d2e7e4" "#979797" "brightblack"  ))
   (base8      '("#f4b4f4" "#f4b4f4" "white"        ))
   (fg         base5)
   (fg-alt     base5)

   (grey       base4)
   (red        '("#993e4c" "#ff6655" "red"          ))
   (orange     '("#87711d" "#dd8844" "brightred"    ))
   (green      '("#4c963e" "#99bb66" "green"        ))
   (teal       '("#3e965b" "#44b9b1" "brightgreen"  ))
   (yellow     '("#96883e" "#F4E409" "yellow"       ))
   (blue       '("#3e4c96" "#51afef" "brightblue"   ))
   (dark-blue  '("#3e4c96" "#2257A0" "blue"         ))
   (magenta    '("#4c963e" "#c678dd" "brightmagenta"))
   (violet     '("#4c963e" "#a9a1e1" "magenta"      ))
   (cyan       '("#3e7996" "#46D9FF" "brightcyan"   ))
   (dark-cyan  '("#3e7996" "#5699AF" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      yellow)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      yellow)
   (builtin        magenta)
   (comments       base3)
   (doc-comments   base3)
   (constants      violet)
   (functions      magenta)
   (keywords       yellow)
   (methods        violet)
   (operators      dark-blue)
   (type           dark-blue)           ;
   (strings        teal)
   (variables      green)              ;
   (numbers        orange)
   (region         `(,(doom-darken (car green) 0.5) ,@(doom-lighten (cdr base1) 0.35)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-base16-apathy-brighter-modeline)
   (-modeline-pad
    (when doom-base16-apathy-padded-modeline
      (if (integerp doom-base16-apathy-padded-modeline) doom-base16-apathy-padded-modeline 4)))

   (modeline-fg     fg)
   (modeline-fg-alt base5)

   (modeline-bg base1)
   (modeline-bg-l `(,(doom-darken (car bg-alt) 0.1) ,@(cdr base0)))
   (modeline-bg-inactive   `(,(doom-darken (car bg-alt) 0.3) ,@(cdr bg-alt)))
   (modeline-bg-inactive-l `(,(car bg-alt) ,@(cdr base1))))


  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   (evil-goggles-default-face :inherit 'region :background (doom-blend region bg 0.5))

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)

   (font-lock-comment-face
    :foreground comments
    :background (if doom-base16-apathy-comment-bg (doom-lighten bg 0.05)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if -modeline-bright base8 highlight))

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   ;; Doom modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)

   ;; ivy-mode
   (ivy-current-match :background dark-blue :distant-foreground base0 :weight 'normal)

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; LaTeX-mode
   (font-latex-math-face :foreground green)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))

   ;; org-mode
   (org-hide :foreground hidden)
   (solaire-org-hide-face :foreground hidden)

   ;; lsp-mode
   (lsp-headerline-breadcrumb-separator-face :foreground green)

   ;; rjsx
   (rjsx-tag :foreground red)
   (rjsx-attr :foreground orange))

  ;; --- extra variables ---------------------
  ()
  )

;;; doom-base16-apathy-theme.el ends here
