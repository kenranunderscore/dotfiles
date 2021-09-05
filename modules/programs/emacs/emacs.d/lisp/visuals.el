;;; Contains customizations that control the look of my Emacs.  That
;;; is, mouse color, theme, font selection and so on.

;; The default cursor is black, which interferes with mostly using a
;; dark theme.  Brighten it up a bit.
(set-mouse-color "white")
(add-hook 'server-after-make-frame-hook
          (lambda () (set-mouse-color "white")))

(defun my--set-evil-state-cursors ()
  "Set the cursor to a box, and use a different color for insert
and emacs mode.  The default color should be the one that the
current theme uses as background for the 'cursor face."
  (let* ((color (face-background 'cursor))
         (default-cursor `(,color box))
         (insert-cursor `("lime green" box)))
    (setq evil-operator-state-cursor default-cursor)
    (setq evil-normal-state-cursor default-cursor)
    (setq evil-replace-state-cursor default-cursor)
    (setq evil-visual-state-cursor default-cursor)
    (setq evil-motion-state-cursor default-cursor)
    (setq evil-emacs-state-cursor insert-cursor)
    (setq evil-insert-state-cursor insert-cursor)
    (remove-hook 'server-after-make-frame-hook
                 #'my--set-evil-state-cursors)))

(defun my--is-initial-daemon-frame-p ()
  "Check whether the selected frame is the one that seems to be
automatically created when the daemon starts.  If this is the
selected frame we don't want to do certain things, like modifying
faces."
  (string= (frame-parameter (selected-frame) 'name) "F1"))

(defun my--switch-font (font)
  "Apply the attributes stored for FONT in `my--font-alist'."
  (interactive
   (list (intern
          (completing-read
           "Font: "
           (mapcar #'car
                   (assoc-delete-all my--current-font
                                     (copy-alist my--font-alist)))))))
  (let* ((attrs (alist-get font my--font-alist))
         (family (plist-get attrs :family))
         (height (plist-get attrs :default-height)))
    (setq my--current-font font)
    (setq my--default-font-height height)
    (set-face-attribute
     'default nil
     :font family
     :weight (plist-get attrs :weight)
     :height (plist-get attrs :default-height))
    (set-face-attribute
     'fixed-pitch nil
     :font family
     :height 1.0)))

;; An alist of my preferred font families, together with a plist of
;; certain attributes that need to be applied when switching to the
;; respective font.
(setq my--font-alist
      '((iosevka-serif . (:family
                          "Iosevka Custom"
                          :default-height
                          160
                          :weight
                          regular))
        (iosevka . (:family
                    "Iosevka"
                    :default-height
                    160
                    :weight
                    regular))
        (hack . (:family
                 "Hack"
                 :default-height
                 150
                 :weight
                 regular))
        (fira . (:family
                 "Fira Code"
                 :default-height
                 140
                 :weight
                 regular))
        (ibm-plex . (:family
                     "IBM Plex Mono"
                     :default-height
                     150
                     :weight
                     regular))
        (cascadia . (:family
                     "Cascadia Code"
                     :default-height
                     150
                     :weight
                     light))))

;; The currently selected font (key of `my--font-alist').  Setting
;; this value only changes the default; it is reset when switching
;; fonts.
(setq my--current-font 'iosevka-serif)

;; Now set all the face attributes, but also register a hook that
;; makes sure that these also work when using the Emacs daemon
;; together with emacsclient.
(my--switch-font my--current-font)
(setq my--has-set-font-in-initial-frame nil)
(add-hook 'server-after-make-frame-hook
          (lambda ()
            (unless my--has-set-font-in-initial-frame
              (setq my--has-set-font-in-initial-frame t)
              (my--switch-font my--current-font))))

;; Try out native ligature support via Harfbuzz composition tables
;; (doesn't work with every font, but works for instance with Fira
;; Code and Iosevka).  See:
;; https://github.com/tonsky/FiraCode/wiki/Emacs-instructions#using-composition-char-table
(let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
               (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
               (36 . ".\\(?:>\\)")
               (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
               (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
               (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
               (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
               (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
               (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
               (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
               (48 . ".\\(?:x[a-zA-Z]\\)")
               (58 . ".\\(?:::\\|[:=]\\)")
               (59 . ".\\(?:;;\\|;\\)")
               (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
               (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
               (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
               (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
               (91 . ".\\(?:]\\)")
               (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
               (94 . ".\\(?:=\\)")
               (119 . ".\\(?:ww\\)")
               (123 . ".\\(?:-\\)")
               (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
               (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)"))))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))

(defun my--switch-theme (name)
  "Switch themes interactively.  Similar to `load-theme' but also
disables all other enabled themes."
  (interactive
   (list (intern
          (completing-read
           "Theme: "
           (mapcar #'symbol-name
                   (-difference (custom-available-themes)
                                custom-enabled-themes))))))
  (progn
    (mapc #'disable-theme
          custom-enabled-themes)
    (load-theme name t)
    (unless (my--is-initial-daemon-frame-p)
      ;; If it's the initial "daemon frame" then hooks in
      ;; `server-after-make-frame-hook' will be executed, including
      ;; one that calls `my--set-evil-state-cursor-colors'.
      (my--set-evil-state-cursors))))
