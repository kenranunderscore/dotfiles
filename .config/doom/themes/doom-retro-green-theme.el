;;; doom-retro-green-theme.el --- inspired by Atom One Dark
(require 'doom-themes)

;;
(defgroup doom-retro-green-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-retro-green-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-one-theme
  :type '(choice integer boolean))

;;
(def-doom-theme doom-retro-green
  "A dark theme inspired by Atom One Dark"

  ;; name        default   256       16
  ((bg         '("#001100" nil       nil            ))
   (bg-alt     '("#001100" nil       nil            ))
   (base0      '("#1B2229" "black"   "black"        ))
   (base1      '("#1c1f24" "#1e1e1e" "brightblack"  ))
   (base2      '("#202328" "#2e2e2e" "brightblack"  ))
   (base3      '("#23272e" "#262626" "brightblack"  ))
   (base4      '("#3f444a" "#3f3f3f" "brightblack"  ))
   (base5      '("#95836f" "#95836f" "brightblack"  ))
   (base6      '("#73797e" "#6b6b6b" "brightblack"  ))
   (base7      '("#9ca0a4" "#979797" "brightblack"  ))
   (base8      '("#DFDFDF" "#dfdfdf" "white"        ))
   (fg         '("#218c23" "#218c23" "brightwhite"  ))
   (fg-alt     '("#218c23" "#218c23" "white"        ))

   (grey       base4)
   (red        '("#ed254e" "#ed254e" "red"          ))
   (orange     '("#e53d00" "#e53d00" "brightred"    ))
   (green      '("#8dcc78" "#8dcc78" "green"        ))
   (teal       '("#46fc32" "#46fc32" "brightgreen"  ))
   (yellow     '("#fffc31" "#fffc31" "yellow"       ))
   (blue       '("#2e86ab" "#2e86ab" "brightblue"   ))
   (dark-blue  '("#2257A0" "#2257A0" "blue"         ))
   (magenta    '("#ed254e" "#ed254e" "brightmagenta"))
   (violet     '("#ed254e" "#ed254e" "magenta"      ))
   (cyan       '("#898989" "#898989" "brightcyan"   ))
   (dark-cyan  '("#005f00" "#005f00" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      teal)
   (vertical-bar   (doom-darken dark-cyan 0.3))
   (selection      vertical-bar)
   (builtin        green)
   (comments       dark-cyan)
   (doc-comments   dark-cyan)
   (constants      green)
   (functions      teal)
   (keywords       teal)
   (methods        green)
   (operators      yellow)
   (type           yellow)
   (strings        cyan)
   (variables      (doom-lighten fg 0.3))
   (numbers        yellow)
   (region         (doom-darken dark-cyan 0.5))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright nil)
   (-modeline-pad nil)

   (modeline-fg     nil)
   (modeline-fg-alt base5)

   (modeline-bg
    (if -modeline-bright
        (doom-darken blue 0.475)
      `(,(doom-darken (car bg-alt) 0.15) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken blue 0.45)
      `(,(doom-darken (car bg-alt) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive   `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg-alt)))
   (modeline-bg-inactive-l `(,(car bg-alt) ,@(cdr base1))))


  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   (evil-goggles-default-face :inherit 'region :background (doom-blend region bg 0.5))

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)

   (font-lock-comment-face
    :foreground comments
    :background bg)
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

   ;; ivy
   (ivy-current-match :background region :distant-foreground teal :weight 'normal)
   (ivy-minibuffer-match-highlight :foreground yellow)
   (ivy-minibuffer-match-face-2 :inherit 'ivy-minibuffer-match-face-1 :foreground yellow :background nil)
   (ivy-minibuffer-match-face-3 :inherit 'ivy-minibuffer-match-face-2 :foreground teal)
   (ivy-minibuffer-match-face-4 :inherit 'ivy-minibuffer-match-face-2 :foreground green)
   (ivy-highlight-face :foreground green)

   ;; which-key
   (which-func :foreground green)
   (which-key-command-description-face :foreground fg)
   (which-key-group-description-face :foreground teal)
   (which-key-local-map-description-face :foreground teal)

   ;; magit
   (magit-branch-current :foreground teal)
   (magit-section-heading :foreground teal :weight 'bold)
   (magit-section-heading-selection :foreground yellow :weight 'bold)
   (magit-section-secondary-heading :foreground green)
   (magit-filename :foreground green)
   (magit-diff-hunk-heading-highlight :foreground bg :background fg)

   ;; company
   (company-tooltip :background (doom-darken region 0.3))
   (company-tooltip-mouse :inherit 'company-tooltip-search-selection)

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   (markdown-code-face :background (doom-lighten base3 0.05))

   ;; UI
   (button :foreground teal :bold t :underline t)
   (custom-button :foreground teal :bold t :underline t :background bg)

   ;; evil
   (evil-ex-search :background dark-cyan :foreground teal)
   (evil-ex-lazy-highlight :background dark-cyan :foreground teal)

   ;; isearch
   (isearch :foreground bg :background fg :weight 'bold)

   ;; dired / diredfl
   (dired-directory :foreground green :bold 'bold)
   (diredfl-dir-heading :foreground teal :weight 'bold)
   (diredfl-dir-name :foreground green :bold 'bold)
   (diredfl-dir-priv :foreground teal)
   (diredfl-number :foreground red)

   ;; org-mode
   (org-level-1 :foreground yellow :bold bold)
   (org-level-2 :foreground teal :bold bold)
   (org-level-3 :foreground green :bold bold)
   (org-level-4 :inherit 'org-level-3)
   (org-level-5 :inherit 'org-level-3)
   (org-level-6 :inherit 'org-level-3)
   (org-hide :foreground hidden)
   (org-todo :foreground teal :bold 'inherit)
   (solaire-org-hide-face :foreground hidden))

  ;; --- extra variables ---------------------
  ()
  )

;;; doom-retro-green-theme.el ends here
