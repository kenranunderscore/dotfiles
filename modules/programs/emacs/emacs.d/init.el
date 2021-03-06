;;;; General settings

;; Since I use home-manager to manage my dotfiles, user environment
;; and in particular Emacs (including packages) I have the guarantee
;; that packages are coming from Nix instead of having to be
;; downloaded.  This is why used to specify :ensure nil in my
;; `use-package' calls.  Now that I've run into lots of problems
;; especially with ELPA packages loaded through nixpkgs I want to have
;; a fallback method.
(setq use-package-always-ensure t)

;; The `general' package allows us to easily define keybindings. This
;; is especially useful for `evil-mode'.
(use-package general)

;; Some additional and changed Emacs keybindings go here (evil- and
;; mode-specific ones can be found in the evil section or the one for
;; the respective mode).
(general-define-key
 :states '(normal visual motion emacs operator)
 "C-h F" 'describe-face
 "C-h M" 'describe-keymap
 "C-h V" 'set-variable)

;;; I do not want customizations done via `customize' to end up in
;;; this file.  Use a separate file instead and load that one on
;;; startup.
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'no-error)

;; I wish to know how fast my Emacs is starting.  I'm not sure how to
;; make use of all that `use-package' has to offer in that regard yet,
;; but I want to at least see when I've made things worse.
(add-hook
 'emacs-startup-hook
 (lambda ()
   (message
    "Emacs startup took %s with %d garbage collections"
    (format
     "%.2f seconds"
     (float-time (time-subtract after-init-time before-init-time)))
    gcs-done)))

;; Disable the graphical UI things like the tool and menu bars, the
;; splash screen, and others.
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(setq inhibit-splash-screen t)

;; The default cursor is black, which interferes with mostly using a
;; dark theme.  Brighten it up a bit.
(set-mouse-color "white")
(add-hook 'server-after-make-frame-hook
          (lambda () (set-mouse-color "white")))

;; We want point to be inside newly opening help buffers so we may
;; quickly close them with q.
(setq help-window-select t)

;; Resize proportionally after deleting windows.
(setq window-combination-resize t)

;; Answering a question with =yes= instead of just =y= is just
;; annoying.
(fset 'yes-or-no-p 'y-or-n-p)

;; Since I cannot ever decide which theme I like best, there are a few
;; themes loaded here.

;; https://protesilaos.com/modus-themes/
(use-package modus-themes
  :defer t)

;; https://github.com/hlissner/emacs-doom-themes
(use-package doom-themes
  :config
  (load-theme 'doom-dracula t)
  :custom
  (doom-dracula-brighter-modeline t))

;;; Font faces and other settings.

;; Tried-and-approved fonts/heights:
;; - Terminus 160
;; - Inconsolata 160
;; - Camingo Code 140
;; - Fira Code 130
;; - Iosevka 140
(setq my/monospace-font "Iosevka")
(setq my/variable-font "Cantarell")
(setq my/default-font-height 140)

(defun my/set-face-attributes ()
  "Set the face attributes for 'default, 'fixed-pitch and
'variable-pitch styles."
  (set-face-attribute
   'default nil
   :font my/monospace-font
   :weight 'regular
   :height my/default-font-height)
  (set-face-attribute
   'fixed-pitch nil
   :font my/monospace-font
   :height 1.0)
  (set-face-attribute
   'variable-pitch nil
   :font my/variable-font
   :height 1.0))

;; Now set all the face attributes, but also register a hook that
;; makes sure that these also work when using the Emacs daemon
;; together with emacsclient.
(my/set-face-attributes)
(add-hook 'server-after-make-frame-hook #'my/set-face-attributes)

;; Enable line numbers in programming modes.
(use-package display-line-numbers
  :hook ((prog-mode . display-line-numbers-mode)
         (conf-mode . display-line-numbers-mode)))

;; Insert newline at the end of files.
(setq require-final-newline t)
(setq mode-require-final-newline t)

;; When using Emacs HEAD (with the merged native-comp branch) a lot of
;; warnings show up during startup and when changing modes.  We could
;; increase the minimum severity for logs to be shown by setting
;; warning-minimum-level to :error or disable the warnings for native
;; compilation entirely like this:
(setq native-comp-async-report-warnings-errors nil)

;; Use spaces for indentation by default.
(setq-default indent-tabs-mode nil)

;;; Vim emulation with evil-mode

;; Having a dedicated leader key (SPC in my case) is one of the most
;; important things to me as it opens up a lot of possibilities for
;; creating custom keymaps.  The keybindings naturally do not clash
;; with the default Emacs-style bindings many packages introduce.  I
;; will use this to try and create more vim-inspired mnemonic
;; keybindings (say, p for project-specific commands, g for git etc.)
(general-create-definer with-leader
  :keymaps 'override
  :states '(normal insert emacs visual motion)
  :prefix "SPC"
  :non-normal-prefix "C-SPC")

;; A local leader key is something that is usually used to access
;; situational commands, for instance language-specific or
;; mode-specific ones.
(general-create-definer with-local-leader
  :prefix ",")

;; This is a list of modes that we do not want the evil mode (defined
;; below) to be enabled in by default. It's mostly a preference of
;; mine to use Emacs mode in REPL, terminal and shell buffers.
(setq my/holy-modes
      '((eshell-mode . eshell)
        (notmuch-hello-mode . nil)
        (racket-repl-mode . nil)
        (racket-stepper-mode . nil)
        (shell-mode . nil)
        (sly-mrepl-mode . nil)
        (term-mode . (term term ansi-term multi-term))
        (haskell-interactive-mode . nil)))

(setq my/evil-holy-modes
      (mapcar #'car my/holy-modes))

(setq my/evil-collection-exemptions
      (remove nil
              (mapcar #'cdr my/holy-modes)))

;; The evil package offers a very complete vim experience inside of
;; Emacs.
(use-package evil
  :config
  (evil-mode 1)
  (dolist (mode my/evil-holy-modes)
    (evil-set-initial-state mode 'emacs))
  (setq evil-insert-state-cursor '(hbar . 3))
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
(use-package evil-collection
  :after evil
  :config
  (dolist (x my/evil-collection-exemptions)
    (delete x evil-collection-mode-list))
  (evil-collection-init)
  (evil-collection-inhibit-insert-state 'notmuch-hello-mode-map)
  :custom
  ((evil-collection-company-use-tng t)
   (evil-collection-want-unimpaired-p nil)))

;; The analogue of Tim Pope's vim-surround plugin in Emacs.  Now I can
;; use things like ysiw) to surround an inner word with non-padded
;; normal parentheses, ds] to delete surrounding brackets, or cd[{ to
;; change surrounding brackets to curly braces with whitespace
;; padding.
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode))

;; Henrik Lissner's evil-snipe replaces the default vim 's' binding by
;; enabling us to search forward/backward incrementally for
;; 2-character sequences.  In addition, evil-snipe-override-mode makes
;; the 'f', 'F', 't', 'T' searches repeatable by pressing the
;; respective key again to jump by one match.  It also adds
;; highlighting to those motions.
(use-package evil-snipe
  :after evil
  :diminish evil-snipe-local-mode
  :config
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1))

;; Evil-org enables me to use evil keybindings in org-agenda.  As a
;; bonus it adds some keybindings and text objects for org files as
;; well.
(use-package evil-org
  :after (evil org)
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; Create nice custom mappings for normal mode (and others) that are
;; accessed with the SPC key.
(with-leader
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
  "b n" 'next-buffer
  "b p" 'previous-buffer
  ;; Toggles
  "t" '(:ignore t :which-key "toggle")
  "t l" '(display-line-numbers-mode :which-key "line numbers")
  ;; Language-agnostic code-related commands
  "c" '(:ignore t :which-key "code")
  "c l" 'comment-line
  "c r" 'comment-or-uncomment-region
  ;; Searching
  "s" '(:ignore t :which-key "search/switch")
  "s g" 'consult-git-grep
  "s p" 'consult-ripgrep
  "s t" 'load-theme
  ;; Window management (redundant)
  "w" '(evil-window-map :which-key "windows"))

;; Enable C-w for window management everywhere.  This means that I
;; need to override the Emacs default binding, which can be done via
;; general's :keymaps 'override.
(general-define-key
 :states '(normal visual motion operator)
 :keymaps 'override
 "C-w" 'evil-window-map
 "C-w C-h" 'evil-window-left
 "C-w C-k" 'evil-window-up
 "C-w C-j" 'evil-window-down
 "C-w C-l" 'evil-window-right
 "C-w C-d" 'evil-quit)

;;;; Package-specific configuration

;; A mode for writing Nix expressions in.
(use-package nix-mode
  :mode "\\.nix\\'")

;;; Markdown
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

;;; I use org-mode for lots of things now, but have only recently
;;; started doing so, hence my configuration is very much a work in
;;; progress.

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

;;; Haskell

;; Provide an interactive mode for writing Haskell.  I can work with a
;; REPL, get feedback and compilation errors shown in the code, and so
;; on.
(use-package haskell-mode
  :custom
  (haskell-process-type 'cabal-repl)
  :hook (haskell-mode . interactive-haskell-mode))

;; Define some keybindings that are local to the
;; interactive-haskell-mode using the local leader key.
(with-local-leader
  :states 'normal
  :keymaps 'interactive-haskell-mode-map
  "e" '(:ignore t :which-key "errors")
  "e f" '(haskell-goto-first-error :which-key "first")
  "e n" '(haskell-goto-next-error :which-key "next")
  "e p" '(haskell-goto-prev-error :which-key "previous")
  "i" '(:ignore t :which-key "imports")
  "i i" '(haskell-navigate-imports-go :which-key "navigate to imports")
  "i r" '(haskell-navigate-imports-return :which-key "return from imports")
  "i a" '(haskell-add-import :which-key "add import"))

;;; Dhall
(use-package dhall-mode
  :mode "\\.dhall\\'")

;;; Docker
(use-package dockerfile-mode
  :defer t)

;;; YAML
(use-package yaml-mode
  :defer t)

;;; Clojure with CIDER
(use-package clojure-mode
  :defer t)

(use-package cider
  :after clojure-mode
  :defer t)

;;; CSV
(use-package csv-mode
  :defer t)

;;; PlantUML
(use-package plantuml-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist
               '("\\.\\(plantuml\\|puml\\)\\'" . plantuml-mode))
  :config
  (setq plantuml-default-exec-mode 'executable))

;;; Common Lisp
(use-package sly
  :defer t
  :config
  (setq inferior-lisp-program "sbcl"))

(use-package sly-asdf
  :defer t)

;;; Racket
(use-package racket-mode
  :defer t
  :hook ((racket-mode . racket-xp-mode)
         (racket-mode . racket-unicode-input-method-enable)
         (racket-repl-mode . racket-unicode-input-method-enable)))

;;; Java (for Crafting Interpreters)
(use-package meghanada
  :defer t
  :init
  (add-hook 'java-mode-hook
            (lambda ()
              (meghanada-mode t)
              (flycheck-mode +1)
              (setq c-basic-offset 2)
              (add-hook 'before-save-hook 'meghanada-code-beautify-before-save))))

;;; LSP integration

;; I've used lsp-mode in the past and while it's nice, I feel like
;; it's more in line with the rest of this configuration to try out
;; something more lightweight and closer to vanilla Emacs.  This is
;; where eglot comes into play.
(use-package eglot
  :hook (haskell-mode . eglot-ensure))

;;; Emacs as e-mail client

;; Used by message-mode.
(setq user-full-name "Johannes Maier")

;; I've tried and used mu4e in the past, but always liked the idea of
;; notmuch better.
(use-package notmuch
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
  (notmuch-always-prompt-for-sender t)
  (notmuch-hello-sections '(notmuch-hello-insert-header
                            notmuch-hello-insert-saved-searches
                            notmuch-hello-insert-alltags
                            notmuch-hello-insert-footer)))

;; To switch identities (which I basically only use to set my work
;; signature based on my From address), I use gnus-alias.
(use-package gnus-alias
  :defer t
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
           ,(concat
             "Johannes Maier\n"
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
(use-package diminish)

;;; Helpful

;; This gives us better and more readable help pages.  We also replace
;; some built-in C-h keybings with helpful-* functions.
(use-package helpful
  :after evil
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key))
  :config
  (evil-set-initial-state 'helpful-mode 'motion))

;;; Projectile
(use-package projectile
  :init
  (projectile-mode +1)
  :custom
  (projectile-switch-project-action #'projectile-commander))

(with-leader
  "p" '(projectile-command-map :which-key "projectile"))

;;; Magit
(use-package magit
  :hook (git-commit-mode . evil-insert-state))

;; magit-todos shows lists of the keywords of hl-todo-mode in
;; magit-status buffers, as well as in a dedicated list of todos
;; accessible with magit-todos-list.  Note: The items have to be
;; followed by a colon (more specifically, check out
;; `magit-todos-keyword-suffix').
(use-package magit-todos
  :after (magit hl-todo)
  :config
  (magit-todos-mode)
  (setq magit-todos-rg-extra-args '("-M 120")))

;; Magit-specific keybindings are useful in a global scope, thus they
;; may be accessed under SPC g.
(with-leader
  "g" '(:ignore t :which-key "git")
  "g s" '(magit-status :which-key "status")
  "g S" '(magit-status-here :which-key "status here")
  "g l" '(magit-log :which-key "log")
  "g f" '(magit-pull-from-upstream :which-key "pull")
  "g p" '(magit-push :which-key "pull")
  "g d" '(magit-diff :which-key "diff")
  "g t" '(magit-todos-list :which-key "todos"))

;;; Smartparens
(use-package smartparens
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

(with-leader
  "c f" 'sp-indent-defun)

;;; evil-cleverparens
(use-package evil-cleverparens
  :diminish evil-cleverparens-mode
  :after smartparens
  :init
  (setq evil-cleverparens-use-s-and-S nil)
  (with-eval-after-load 'evil-cleverparens
    (general-define-key
     :states '(normal operator visual)
     :keymaps 'evil-cleverparens-mode-map
     "}" 'evil-forward-paragraph
     "{" 'evil-backward-paragraph
     "M-<" 'evil-cp-next-opening
     "M->" 'evil-cp-previous-closing))
  (add-hook 'smartparens-strict-mode-hook #'evil-cleverparens-mode))

;;; Incremental narrowing

;; I started with helm in Spacemacs, then later switched to Doom Emacs
;; where after a while I tried out ivy and loved it.  Now I want to
;; check out some of the new, more light-weight packages like
;; selectrum and vertico.
(use-package vertico
  :init
  (vertico-mode +1)
  :custom
  (vertico-cycle t)
  :bind
  (:map vertico-map
        ("C-j" . vertico-next)
        ("C-k" . vertico-previous)))

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

(use-package orderless
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
(use-package consult
  :bind (;; C-x bindings
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ;; C-h bindings (help)
         ("C-h a" . consult-apropos)
         ;; M-g bindings (goto)
         ("M-g e" . consult-compile-error)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
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

;;; Embark
(use-package embark
  :bind (("C-," . embark-act)
         ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook (embark-collect-mode . embark-consult-preview-minor-mode))

;;; company

;; Company provides auto-completions in nearly every context.
(use-package company
  :hook ((after-init . global-company-mode))
  :diminish company-mode
  :init
  ;; Without this orderless would be used by default for company
  ;; completions.  It doesn't fit there as well though.
  (define-advice company-capf
      (:around (orig-fun &rest args) set-completion-styles)
    (let ((completion-styles '(basic partial-completion)))
      (apply orig-fun args)))
  :custom
  ((company-idle-delay 0)
   (company-selection-wrap-around t)))

;;; Highlight "todo", "fixme" and other keywords everywhere.
(use-package hl-todo
  :init
  (add-hook 'after-init-hook 'global-hl-todo-mode))

;;; which-key

;; When pressing the first key in a hotkey chain, show a popup that
;; displays the possible completions and associated functions.
(use-package which-key
  :defer t
  :custom
  (which-key-idle-delay 0.3)
  :diminish which-key-mode
  :init
  (add-hook 'after-init-hook 'which-key-mode))

;;; all-the-icons

;; Attach beautiful symbols to, for instance, file names in a dired or
;; ibuffer buffer.
(use-package all-the-icons)

(use-package all-the-icons-dired
  :defer t
  :diminish all-the-icons-dired-mode
  :init
  (add-hook 'dired-mode-hook #'all-the-icons-dired-mode))

(use-package all-the-icons-ibuffer
  :defer t
  :init
  (all-the-icons-ibuffer-mode 1))

;;; Marginalia

;; Annotate minibuffer completions, like showing the bound keys and
;; docstrings for commands in M-x, variable values in "C-h v", file
;; sizes and permissions in "C-x C-f", and much more.
(use-package marginalia
  :init
  (marginalia-mode)
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode)
                           (selectrum-exhibit 'keep-selected))))
  :config
  (setq marginalia-annotators
        '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :bind
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle)))

;;; envrc

;; Let's try out Steve Purcell's envrc package as an alternative to
;; direnv-mode.  Note: this should probably be one of the last modes
;; to load, as the hook function is then placed before the other modes
;; to ensure direnv integration is working as expected.
(use-package envrc
  :defer t
  :init (envrc-global-mode))

;;; ripgrep
(use-package ripgrep
  :defer t)

;;; ace-window
(use-package ace-window
  :defer t
  :init
  (setq aw-keys '(?i ?n ?e ?a ?h ?t ?s ?r))
  :config
  (set-face-attribute 'aw-leading-char-face nil :height 2.5))

(general-define-key
 :states '(normal visual operator motion)
 :keymaps 'override
 "C-w C-w" 'ace-window)

;; C-l is a normal Emacs hotkey that I don't need or use because of
;; evil, and in the modes that I've disabled evil for I don't use it
;; either.  It thus seems like a good choice to have as a fallback for
;; window management, as especially in terminal or shell modes I like
;; being able to use C-w like in a terminal or in vim.  I used to bind
;; 'evil-window-map to C-l, but perhaps it's also a good key to have
;; 'ace-window on as in most cases it's what I'd use anyway.
(general-define-key
 :states '(normal visual motion operator insert emacs)
 :keymaps 'override
 "C-l" 'ace-window)

;;; hydra
(use-package hydra)

;;; default-text-scale
(use-package default-text-scale
  :defer t
  :after hydra)

(defhydra hydra-global-zoom (:hint nil :timeout 3)
  "
  Change the font size globally.\n
  _g_: increase
  _l_: decrease\n
  "
  ("g" default-text-scale-increase)
  ("l" default-text-scale-decrease)
  ("r" (lambda ()
         (interactive)
         (setq default-text-scale--complement 0)
         (set-face-attribute 'default nil
                             :height my/default-font-height))
   "reset" :color blue)
  ("q" nil "exit"))

(with-leader
  "s s" '(hydra-global-zoom/body :which-key "font zoom"))

;;; Built-in packages

;; Diminish only.
(use-package face-remap
  :ensure nil
  :diminish buffer-face-mode)

;; Diminish only.
(use-package autorevert
  :ensure nil
  :diminish auto-revert-mode)

;; Diminish only.
(use-package simple
  :ensure nil
  :diminish auto-fill-function)

;; Diminish only.
(use-package eldoc
  :ensure nil
  :diminish eldoc-mode)

(use-package dired
  :ensure nil
  :defer t
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  :custom
  ;; Sort directories to the top
  (dired-listing-switches "-la --group-directories-first"))

;; Beautify dired a bit.
(use-package diredfl
  :defer t
  :after dired
  :hook (dired-mode . diredfl-mode))

;; Edits in a grep buffer are applied sed-style.
(use-package wgrep
  :defer t
  :custom
  ((wgrep-auto-save-buffer t)
   (wgrep-change-readonly-file nil)
   (wgrep-too-many-file-length 15)))

;; Start `gcmh-mode' for better GC behavior.
(use-package gcmh
  :diminish gcmh-mode
  :init
  (gcmh-mode 1))
