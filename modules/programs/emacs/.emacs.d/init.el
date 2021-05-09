;; Emacs startup can be significantly sped up by reducing the number
;; of garbage collections that take place during initialization.  The
;; default `gc-cons-threshold' of 80 kilobytes is way too low for any
;; more recent system.  Still it's beneficial to reset this temporary
;; value back to a lower number after initialization.  That way the GC
;; pause won't be as long when working within Emacs.
(setq gc-cons-threshold (* 100 1000 1000))

;;;; General settings

;; Since I use home-manager to manage my dotfiles, user environment
;; and in particular Emacs (including packages) I have the guarantee
;; that those packages are coming from Nix instead of having to be
;; downloaded.  This is why I usually specify :ensure nil in my
;; `use-package' calls.  Maybe this is bad, but I'm sticking with it
;; for now. Hence this little macro comes in handy:

(defmacro use-package! (package-name &rest args)
  "Like use-package but prepends an :ensure value of nil on systems
   that I manage with Nix, and t otherwise (on Windows,
   basically)."
  (declare (indent defun))
  `(use-package ,package-name
     :ensure ,(eq system-type 'windows-nt)
     ,@args))

;; The `general' package allows us to easily define keybindings. This
;; is especially useful for `evil-mode'.

(use-package! general)

;;; I do not want customizations done via `customize' to end up in
;;; this file.  Use a separate file instead.

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'no-error)

;; I wish to know how fast my Emacs is starting.  I'm not sure how to
;; make use of all that `use-package' has to offer in that regard yet,
;; but I want to at least see when I've made things worse.

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs startup took %s with %d garbage collections"
                     (format "%.2f seconds"
                             (float-time (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Disable the graphical UI things like the menu bar, the splash
;; screen, and others.

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

(setq inhibit-splash-screen t)

;; The default cursor is black, which interferes with mostly using a
;; dark theme.  Brighten it up a bit.

(set-mouse-color "white")
(add-hook 'server-after-make-frame-hook (lambda () (set-mouse-color "white")))

;; We want point to be inside newly opening help buffers so we may
;; quickly close them with q.

(setq help-window-select t)

;; Since I cannot ever decide which theme I like best, there are a few
;; themes, or collections thereof, installed in my nix configuration:
;;
;; - https://protesilaos.com/modus-themes/
;; - https://github.com/hlissner/emacs-doom-themes
;; - https://github.com/emacs-jp/replace-colorthemes

(use-package! modus-vivendi-theme
  :config (load-theme 'modus-vivendi t))

;; (use-package! doom-themes
;;   :config (load-theme 'doom-sourcerer t))

;; (use-package! color-theme-modern
;;   :config
;;   (load-theme 'taming-mr-arneson t t)
;;   (enable-theme 'taming-mr-arneson))

;; Answering a question with =yes= instead of just =y= is just
;; annoying.

(fset 'yes-or-no-p 'y-or-n-p)

;; Font faces and setting.

(defvar my/monospace-font "Hack")
(defvar my/variable-font "Cantarell")

(defun my/set-face-attributes ()
  (set-face-attribute 'default nil :font my/monospace-font :height 120)
  (set-face-attribute 'fixed-pitch nil :font my/monospace-font :height 0.8)
  (set-face-attribute 'variable-pitch nil :font my/variable-font :height 1.2))

(my/set-face-attributes)
(add-hook 'server-after-make-frame-hook #'my/set-face-attributes)

;; Enable line numbers in programming modes.

(use-package! display-line-numbers
  :hook ((prog-mode . display-line-numbers-mode))
  (conf-mode . display-line-numbers-mode))

;; Insert newline at the end of files.

(setq require-final-newline t)
(setq mode-require-final-newline t)

;; When using the native-comp branch that is soon to be merged into
;; the main Emacs branch a lot of warnings show up during startup and
;; changing modes.  We could increase the minimum severity for logs to
;; be shown by setting warning-minimum-level to :error or disable the
;; warnings for native compilation entirely like this:

(setq comp-async-report-warnings-errors nil)

;; Use spaces for indentation by default.

(setq-default indent-tabs-mode nil)

;;;; Package-specific configuration

;;; Nix expressions

(use-package! nix-mode
  :mode "\\.nix\\'")

;;; Markdown

(use-package! markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

;;; Org-mode

;; When using variable-pitch-mode, all the faces are using the
;; configured font face.  But inside code blocks, verbatim text, meta
;; lines, etc. a fixed-pitch face is what we want to be using.

(defun my/setup-org-fonts ()
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-block-begin-line nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch)))

;; To fix org-indent-mode not indenting variable-pitch fonts nicely,
;; we'd like to use the fixed-pitch font for that as well.
;; Unfortunately setting the face attribute as above didn't work as
;; expected and it seems necessary to do it like this.

(use-package! org-indent
  :diminish org-indent-mode)

(with-eval-after-load 'org-indent
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch)))

;; When writing text in org-mode, auto-fill-mode should be enable to
;; automatically break overly long lines into smaller pieces when
;; typing.  We may still use M-q to re-fill paragraph when editing
;; text.  After loading org-mode, we then run our custom font setup.

(use-package! org
  :hook ((org-mode . variable-pitch-mode)
         (org-mode . auto-fill-mode)
         (org-trigger . save-buffer))
  :custom ((org-startup-indented t)
           (org-ellipsis " ▾")
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
           (org-todo-keywords '((sequence
                                 "TODO(t)"
                                 "WAITING(w)"
                                 "|"
                                 "DONE(d)"
                                 "CANCELLED(c)"))))
  :bind (("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  :config
  (my/setup-org-fonts)
  (advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers))))

;; The org-bullets packages enables us to use UTF-8 characters for the
;; bullet points in org headers.

(use-package! org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Render unordered list bullet points as dots instead of minus/plus.

(font-lock-add-keywords
 'org-mode
 '(("^ *\\([-+]\\) "
    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; The first few levels of org headers should be scaled to be a bit
;; larger than the default text.

(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.15)
                (org-level-3 . 1.1)
                (org-level-4 . 1.05)
                (org-level-5 . 1.0)
                (org-level-6 . 1.0)
                (org-level-7 . 1.0)
                (org-level-8 . 1.0)))
  (set-face-attribute (car face) nil
                      :font my/variable-font
                      :height (cdr face)
                      :weight 'regular))

;; For short presentations org-present looks like it is a good option.

(use-package! org-present
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

;;; Haskell

(use-package! haskell-mode
  :custom
  (haskell-process-type 'cabal-repl)
  :hook (haskell-mode . interactive-haskell-mode))

;;; Dhall

(use-package! dhall-mode
  :mode "\\.dhall\\'")

;;; Docker

(use-package! dockerfile-mode)

;;; YAML

(use-package! yaml-mode)

;;; Clojure

(use-package! clojure-mode)

;;; CSV

(use-package! csv-mode)

;;; LSP integration

;; I've used lsp-mode in the past and while it's nice, I feel like
;; it's more in line with the rest of this configuration to try out
;; something more lightweight and closer to vanilla Emacs.  This is
;; where eglot comes into play.

(use-package! eglot
  :hook (haskell-mode . eglot-ensure))

;;; Vim emulation with evil-mode

;; Having a dedicated leader key (SPC in my case) is one of the most
;; important things to me as it opens up a lot of possibilities for
;; creating custom keymaps.  The keybindings naturally do not clash
;; with the default Emacs-style bindings many packages introduce.  I
;; will use this to try and create more vim-inspired mnemonic
;; keybindings (say, p for project-specific commands, g for git etc.)

(general-create-definer with-leader
  :prefix "SPC")

;; The evil package offers a very complete vim experience inside of
;; Emacs.

(use-package! evil
  :config
  (evil-mode 1)
  :custom
  ((evil-want-C-u-scroll t)
   (evil-want-C-u-delete nil)
   (evil-want-C-w-delete t)
   (evil-want-Y-yank-to-eol t)
   (evil-undo-system 'undo-redo))
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil))

;; This package makes it possible to enable evil-mode (and therefore
;; have a more vim-ish feel) in lots of (mostly minor) modes.  I'm not
;; sure whether I wish to use all of these (I think I don't need evil
;; in shells and REPLs), but I'll give them a try.

(use-package! evil-collection
  :after evil
  :config
  (evil-collection-init))

;; The analogue of [[https://github.com/tpope/vim-surround][Tim Pope's
;; vim-surround plugin]] in Emacs.  Now I can use things like ysiw) to
;; surround an inner word with non-padded normal parentheses, ds] to
;; delete surrounding brackets, or cd[{ to change surrounding brackets
;; to curly braces with whitespace padding.

(use-package! evil-surround
  :after evil
  :config
  (global-evil-surround-mode))

;; Henrik Lissner's evil-snipe replaces the default vim 's' binding by
;; enabling us to search forward/backward incrementally for
;; 2-character sequences.  In addition, evil-snipe-override-mode makes
;; the 'f', 'F', 't', 'T' searches repeatable by pressing the
;; respective key again to jump by one match.  It also adds
;; highlighting to those motions.

(use-package! evil-snipe
  :after evil
  :diminish evil-snipe-local-mode
  :config
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1))

;; Create nice custom mappings for normal mode (and others) that are
;; accessed with the SPC key.

(with-leader
  :states '(normal visual)
  ;; Give SPC SPC one more chance
  "SPC" '(execute-extended-command :which-key "M-x")
  ;; Different ways to quit Emacs
  "q" '(:ignore t :which-key "quit")
  "q f" 'evil-save-and-quit
  "q k" 'save-buffers-kill-emacs
  ;; Buffer-related commands
  "b" '(:ignore t :which-key "buffer")
  "b b" 'consult-buffer
  "b q" 'kill-this-buffer
  "b i" 'ibuffer
  "b k" 'kill-buffer
  ;; Toggles
  "t" '(:ignore t :which-key "toggle")
  "t l" '(display-line-numbers-mode :which-key "line numbers")
  ;; Language-agnostic code-related commands
  "c" '(:ignore t :which-key "code")
  "c l" 'comment-line
  "c r" 'comment-region
  ;; Searching
  "s g" 'consult-git-grep
  "s p" 'consult-ripgrep)

(general-define-key
 :states '(normal visual motion)
 "C-w C-h" 'evil-window-left
 "C-w C-k" 'evil-window-up
 "C-w C-j" 'evil-window-down
 "C-w C-l" 'evil-window-right
 "C-w C-d" 'evil-quit)

;;; Emacs as e-mail client

;; I've tried and used mu4e in the past, but always liked the idea of
;; notmuch better.  I'll give notmuch a shot now that I have a working
;; syncthing setup to sync my tag database.

(setq user-full-name "Johannes Maier")

(use-package! notmuch
  :defer t
  :config
  (setq user-mail-address "johannes.maier@mailbox.org")
  :custom
  (message-send-mail-function 'message-send-mail-with-sendmail)
  (message-kill-buffer-on-exit t)
  (message-sendmail-envelope-from 'header)
  (mail-envelope-from 'header)
  (mail-specify-envelope-from 'header)
  (mail-user-agent 'message-user-agent)
  (notmuch-show-all-multipart/alternative-parts nil)
  (notmuch-always-prompt-for-sender t))

;; To switch identities (which I basically only use to set my work
;; signature based on my From address), I use gnus-alias.

(use-package! gnus-alias
  :config
  (setq gnus-alias-identity-alist
        `(("mailbox"
           nil
           "Johannes Maier <johannes.maier@mailbox.org>"
           nil
           nil
           nil
           nil)
          ("ag"
           nil
           "Johannes Maier <johannes.maier@active-group.de>"
           "Active Group GmbH"
           nil
           nil
           ,(concat "Johannes Maier\n"
                    "johannes.maier@active-group.de\n\n"
                    "+49 (7071) 70896-67\n\n"
                    "Active Group GmbH\n"
                    "Hechinger Str. 12/1\n"
                    "72072 Tübingen\n"
                    "Registergericht: Amtsgericht Stuttgart, HRB 224404\n"
                    "Geschäftsführer: Dr. Michael Sperber"))))
  (setq gnus-alias-default-identity "mailbox")
  (setq gnus-alias-identity-rules
        '(("ag" ("any" "@active-group.de" both) "ag")))
  :init
  (add-hook 'message-setup-hook 'gnus-alias-determine-identity))

;;; Diminish

;; The diminish package enables us to hide minor modes from the mode
;; line.  It's especially useful for certain modes that are globally
;; enabled anyway.  Use-package has built-in support for it available
;; with the :diminish keyword.

(use-package! diminish)

;;; Helpful

;; This gives us better and more readable help pages.  We also replace
;; some built-in C-h keybings with helpful-* functions.

(use-package! helpful
  :after evil
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key))
  :config
  (evil-set-initial-state 'helpful-mode 'motion))

;;; Projectile

(use-package! projectile
  :init
  (projectile-mode +1))

(with-leader
  :states '(normal visual)
  "p" '(projectile-command-map :which-key "projectile"))

;;; Magit

(use-package! magit)

(with-leader
  :states '(normal visual)
  "g" '(:ignore t :which-key "git")
  "g s" '(magit-status :which-key "status")
  "g S" '(magit-status-here :which-key "status here")
  "g l" '(magit-log :which-key "log")
  "g f" '(magit-pull-from-upstream :which-key "pull")
  "g p" '(magit-push :which-key "pull")
  "g d" '(magit-diff :which-key "diff"))

;;; Smartparens

(use-package! smartparens
  :diminish smartparens-mode
  :config
  (sp-pair "'" nil :actions nil)
  :bind (("C-M-a" . sp-beginning-of-sexp)
         ("C-M-e" . sp-end-of-sexp)
         ("C-<down>" . sp-down-sexp)
         ("C-<up>" . sp-up-sexp)
         ("M-<down>" . sp-backward-down-sexp)
         ("M-<up>" . sp-backward-up-sexp)
         ("C-M-f" . sp-forward-sexp)
         ("C-M-b" . sp-backward-sexp)
         ("C-M-n" . sp-next-sexp)
         ("C-M-p" . sp-previous-sexp)
         ;; TODO forward/backward symbol?
         ("C-<right>" . sp-forward-slurp-sexp)
         ("C-<left>" . sp-backward-slurp-sexp)
         ("M-<right>" . sp-forward-barf-sexp)
         ("M-<left>" . sp-backward-barf-sexp)
         ("C-M-t" . sp-transpose-sexp)
         ("C-M-k" . sp-kill-sexp)
         ("C-k" . sp-kill-hybrid-sexp)
         ("M-k" . sp-backward-kill-sexp)
         ("C-M-w" . sp-copy-sexp)
         ("C-M-d" . sp-delete-sexp)
         ;; TODO wrap with parens/brackets/braces/...
         )
  :init
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  ;; Enable smartparens-strict-mode for all LISP modes listed in
  ;; sp-lisp-modes.
  (mapc
   (lambda (mode)
     (add-hook
      (intern (format "%s-hook" (symbol-name mode)))
      'smartparens-strict-mode))
   sp-lisp-modes))

;;; Incremental narrowing

;; I started with helm in Spacemacs, then later switched to Doom Emacs
;; where after a while I tried out ivy and loved it.  Now I want to
;; check out some of the new, more light-weight packages like
;; selectrum and vertico.

(use-package! vertico
  :init
  (vertico-mode +1))

;;; Orderless

;; orderless is a completion style that fits in very well with
;; selectrum.  Parts of a search string may match according to several
;; matching styles.  We want to be able to specify which matching
;; style to use by appending a suffix so a search string.  Therefore
;; we define style dispatchers and use them to customize
;; orderless-style-dispatchers.

;; Prepending an equals sign to a search term will search for literal
;; matches of the preceding string.
(defun my/literal-if-= (pattern _index _total)
  (when (string-prefix-p "=" pattern)
    `(orderless-literal . ,(substring pattern 1))))

;; A prepended bang discards everything that matches the preceding
;; literal string.
(defun my/without-if-! (pattern _index _total)
  (when (string-prefix-p "!" pattern)
    `(orderless-without-literal . ,(substring pattern 1))))

;; The tilde sign gives me a way to have "fuzzy" search, if needed.
(defun my/flex-if-~ (pattern _index _total)
  (when (string-prefix-p "~" pattern)
    `(orderless-flex . ,(substring pattern 1))))

(use-package! orderless
  :custom (completion-styles '(orderless))
  (orderless-style-dispatchers
   '(my/literal-if-=
     my/without-if-!
     my/flex-if-~)))

;;; Consult

;; The consult package is the analogue of counsel, which I used for
;; quite some time, though not in any extent close to full.  This
;; defines some basic bindings mostly taken from an example in its
;; readme.

(use-package! consult
  :after project
  :bind (;; C-x bindings
         ("C-x b" . consult-buffer)                ;; was switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; was switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; was switch-to-buffer-other-frame
         ;; C-h bindings (help)
         ("C-h a" . consult-apropos)
         ;; M-g bindings (goto)
         ("M-g e" . consult-compile-error)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)           ;; easier to type
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-project-imenu)
         ;; M-s bindings (search)
         ("M-s f" . consult-find)
         ("M-s L" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines))
  :custom
  (consult-project-root-function
   (lambda ()
     (when-let (project (project-current))
       (project-root project)))))
;; TODO other isearch integration?
;; TODO :init narrowing, preview delay

;;; Corfu

(use-package! corfu
  :config
  (corfu-global-mode)
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete))

;;; hl-todo

(use-package! hl-todo
  :init
  (add-hook 'after-init-hook 'global-hl-todo-mode))

;;; which-key

;; When pressing the first key in a hotkey chain, show a popup that
;; displays the possible completions and associated functions.

(use-package! which-key
  :custom
  (which-key-idle-delay 0.3)
  :diminish which-key-mode
  :init
  (add-hook 'after-init-hook 'which-key-mode))

;;; all-the-icons

;; Attach beautiful symbols to, for instance, file names in a dired or
;; ibuffer buffer.

(use-package! all-the-icons)

(use-package! all-the-icons-dired
  :diminish all-the-icons-dired-mode
  :init
  (add-hook 'dired-mode-hook #'all-the-icons-dired-mode))

(use-package! all-the-icons-ibuffer
  :init
  (all-the-icons-ibuffer-mode 1))

;;; Marginalia

;; Annotate minibuffer completions, like showing the bound keys and
;; docstrings for commands in M-x, variable values in "C-h v", file
;; sizes and permissions in "C-x C-f", and much more.

(use-package! marginalia
  :init
  (marginalia-mode)
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode)
                           (selectrum-exhibit 'keep-selected))))
  :config
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :bind
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle)))

;;; envrc

;; Let's try out Steve Purcell's envrc package as an alternative to
;; direnv-mode.  Note: this should probably be one of the last modes
;; to load, as the hook function is then placed before the other modes
;; to ensure direnv integration is working as expected.

(use-package! envrc
  :init (envrc-global-mode))

;;; ripgrep

(use-package! ripgrep)

;;; Built-in packages

;; Diminish only.
(use-package! face-remap
  :diminish buffer-face-mode)

;; Diminish only.
(use-package! autorevert
  :diminish auto-revert-mode)

;; Diminish only.
(use-package! simple
  :diminish auto-fill-function)

(use-package! dired
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  :custom
  ;; Sort directories to the top
  (dired-listing-switches "-la --group-directories-first"))

;; Beautify dired a bit.
(use-package! diredfl
  :hook (dired-mode . diredfl-mode))

;; Reduce the GC threshold again
(setq gc-cons-threshold (* 5 1000 1000))
