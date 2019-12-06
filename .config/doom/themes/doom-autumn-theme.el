;;; doom-autumn-theme.el --- inspired by Atom One Dark
(require 'doom-themes)

;;
(defgroup doom-autumn-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-autumn-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-autumn-theme
  :type '(choice integer boolean))

;;
(def-doom-theme doom-autumn
  "A dark theme inspired by Atom One Dark"

  ;; name        default   256       16
  ((bg         '("#191005" nil       nil            ))
   (bg-alt     '("#191005" nil       nil            ))
   (base0      '("#1B2229" "black"   "black"        ))
   (base1      '("#1c1f24" "#1e1e1e" "brightblack"  ))
   (base2      '("#202328" "#2e2e2e" "brightblack"  ))
   (base3      '("#23272e" "#262626" "brightblack"  ))
   (base4      '("#3f444a" "#3f3f3f" "brightblack"  ))
   (base5      '("#95836f" "#95836f" "brightblack"  ))
   (base6      '("#73797e" "#6b6b6b" "brightblack"  ))
   (base7      '("#9ca0a4" "#979797" "brightblack"  ))
   (base8      '("#DFDFDF" "#dfdfdf" "white"        ))
   (fg         '("#4f7410" "#ff7000" "brightwhite"  ))
   (fg-alt     '("#4f7410" "#ff7000" "white"        ))

   (grey       '("#707a6a" "#505a4a" "brightblack"  ))
   (red        '("#ff4e00" "#ff4e00" "red"          ))
   (orange     '("#ff7000" "#ff7000" "brightred"    ))
   (green      '("#7cb518" "#7cb518" "green"        ))
   (teal       '("#dbc077" "#dbc077" "brightgreen"  ))
   (yellow     '("#ffbf00" "#ffbf00" "yellow"       ))
   (blue       '("#0075c4" "#0075c4" "brightblue"   ))
   (dark-blue  '("#0075c4" "#0075c4" "blue"         ))
   (magenta    '("#d72638" "#d72638" "brightmagenta"))
   (violet     '("#76597b" "#76597b" "magenta"      ))
   (cyan       '("#898989" "#898989" "brightcyan"   ))
   (dark-cyan  '("#4f7410" "#4f7410" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      yellow)
   (vertical-bar   (doom-darken grey 0.4))
   (selection      orange)
   (builtin        yellow)
   (comments       grey)
   (doc-comments   grey)
   (constants      orange)
   (functions      orange)
   (keywords       red)
   (methods        red)
   (operators      yellow)
   (type           green)
   (strings        (doom-darken teal 0.1))
   (variables      green)
   (numbers        teal)
   (region         (doom-darken dark-cyan 0.7))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-pad nil)

   (modeline-fg     green)
   (modeline-fg-alt vertical-bar)

   (modeline-bg `(,(car (doom-darken green 0.80)) ,@(cdr base0)))
   (modeline-bg-l `(,(car (doom-darken green 0.80)) ,@(cdr base0)))
   (modeline-bg-inactive   `(,(doom-darken (car bg) 0.2) ,@(cdr base0)))
   (modeline-bg-inactive-l `(,(doom-darken (car bg) 0.2) ,@(cdr base0))))

  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   (evil-goggles-default-face :inherit 'region :background (doom-blend region bg 0.5))

   ((line-number &override) :foreground vertical-bar)
   ((line-number-current-line &override) :foreground orange)

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
   (mode-line-emphasis :foreground highlight)

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   ;; Doom modeline
   (doom-modeline-bar :background highlight)
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)
   (doom-modeline-project-dir :bold t :foreground orange)

   ;; ivy
   (ivy-current-match :background region :distant-foreground teal :weight 'normal)
   (ivy-minibuffer-match-highlight :foreground yellow)
   (ivy-minibuffer-match-face-2 :inherit 'ivy-minibuffer-match-face-1 :foreground yellow :background nil)
   (ivy-minibuffer-match-face-3 :inherit 'ivy-minibuffer-match-face-2 :foreground orange)
   (ivy-minibuffer-match-face-4 :inherit 'ivy-minibuffer-match-face-2 :foreground magenta)
   (ivy-highlight-face :foreground green)

   ;; which-key
   (which-func :foreground green)
   (which-key-command-description-face :foreground fg)
   (which-key-group-description-face :foreground yellow)
   (which-key-local-map-description-face :foreground yellow)

   ;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground green)
   (rainbow-delimiters-depth-2-face :foreground yellow)
   (rainbow-delimiters-depth-3-face :foreground teal)
   (rainbow-delimiters-depth-4-face :foreground blue)
   (rainbow-delimiters-depth-5-face :foreground magenta)
   (rainbow-delimiters-depth-6-face :foreground green)
   (rainbow-delimiters-depth-7-face :foreground yellow)

   ;; magit
   (magit-branch-current :foreground yellow)
   (magit-branch-remote :foreground orange)
   (magit-section-heading :foreground blue :weight 'bold)
   (magit-section-heading-selection :foreground yellow :weight 'bold)
   (magit-section-secondary-heading :foreground green)
   (magit-filename :foreground green)
   (magit-diff-hunk-heading-highlight :foreground bg :background fg)
   (magit-branch-local :foreground yellow)
   (magit-diff-file-heading :weight 'regular)

   ;; company
   (company-tooltip :background (doom-darken region 0.1))
   (company-tooltip-search-selection :foreground yellow)
   (company-tooltip-mouse :inherit 'company-tooltip-search-selection)
   (company-tooltip-selection :inherit 'company-tooltip-search-selection)
   (company-tooltip-annotation :foreground (doom-darken blue 0.2))
   (company-tooltip-annotation-selection :foreground blue)

   ;; navigation
   (nav-flash-face :background fg :foreground yellow)

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
   (evil-ex-search :background fg :foreground bg)
   (evil-ex-lazy-highlight :background fg :foreground bg)

   ;; isearch
   (isearch :foreground bg :background fg :weight 'bold)

   ;; dired / diredfl
   (dired-directory :foreground green :bold 'bold)
   (diredfl-dir-heading :foreground yellow :weight 'bold)
   (diredfl-dir-name :foreground green :bold 'bold)
   (diredfl-dir-priv :foreground teal)
   (diredfl-number :foreground red)

   ;; lsp
   (lsp-face-highlight-read :background (doom-darken blue 0.6))
   (lsp-face-highlight-write :background (doom-darken blue 0.6))
   (lsp-face-highlight-textual :background (doom-darken blue 0.6))

   ;; org-mode
   (org-level-1 :foreground blue :bold t)
   (org-level-2 :foreground yellow :bold t)
   (org-level-3 :foreground magenta :bold t)
   (org-level-4 :foreground red :bold t)
   (org-level-5 :foreground orange :bold t)
   (org-level-6 :foreground violet :bold t)
   (org-hide :foreground hidden)
   (org-todo :foreground strings :bold 'inherit)
   (solaire-org-hide-face :foreground hidden))

  ;; --- extra variables ---------------------
  ()
  )

;;; doom-autumn-theme.el ends here
