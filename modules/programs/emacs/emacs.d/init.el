;; Even though I'm specifying the Emacs packages I'm using via
;; home-manager/nixpkgs, it's still useful to have MELPA accessible
;; directly in case something's broken in nixpkgs.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Install use-package if it's not installed already.  The nixpkgs
;; version of Emacs on my systems already contains most packages,
;; including use-package itself, but when self-compiling Emacs or on
;; Windows I rely on use-package for the installation of all the
;; packages.
(dolist (package '(use-package))
   (unless (package-installed-p package)
       (package-install package)))

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

(defmacro kenran/load-config-file (file)
  "Load FILE relative to the .emacs.d directory."
  `(load (locate-user-emacs-file ,file)
         'no-error))

;; I do not want customizations done via `customize' to end up in
;; this file.  Use a separate file instead and load that one on
;; startup.
(let ((my-custom-file (locate-user-emacs-file "custom.el")))
  (setq custom-file my-custom-file)
  (load custom-file 'no-error))

(defun kenran/open-init-file ()
  "Open my init.el file."
  (interactive)
  (find-file (file-truename (locate-user-emacs-file "init.el"))))

;; Where the custom Elisp files reside.
(setq kenran/lisp-dir (concat user-emacs-directory "lisp/"))

;; Where custom themes reside.
(setq custom-theme-directory (concat kenran/lisp-dir "themes/"))

(defun kenran/open-other-config-file (file)
  "Open FILE of the config files in the lisp directory."
  (interactive
   (list
    (completing-read
     "Config file: "
     (mapcar #'f-filename
             (f-files kenran/lisp-dir)))))
  (find-file (file-truename (concat kenran/lisp-dir file))))

(defun kenran/open-custom-theme-file (file)
  "Open FILE of the themes in `custom-theme-directory'."
  (interactive
   (list
    (completing-read
     "Theme: "
     (mapcar #'f-filename
             (f-files custom-theme-directory)))))
  (find-file (file-truename (concat custom-theme-directory file))))

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

;; Ignore the X resources.  Now Emacs doesn't use the terminal
;; background/foreground colors.
(setq inhibit-x-resources t)

;; We want point to be inside newly opening help buffers so we may
;; quickly close them with q.
(setq help-window-select t)

;; Same for apropos buffers.
(add-hook 'apropos-mode-hook
          (defun kenran/focus-apropos-buffer ()
            (pop-to-buffer (current-buffer))))

;; Resize proportionally after deleting windows.
(setq window-combination-resize t)

;; Answering a question with =yes= instead of just =y= is just
;; annoying.
(fset 'yes-or-no-p 'y-or-n-p)

;; Make commands shown with M-x depend on the active major mode.
;; Note: doesn't work correctly yet, as (command-modes 'some-command)
;; seems to return the modes in an unexpected format.
(setq read-extended-command-predicate
      #'command-completion-default-include-p)

;; The cursor doesn't need to be blinking when it's distinctive
;; enough.
(blink-cursor-mode -1)

;; Load themes and other improvements over the default look.
(kenran/load-config-file "lisp/visuals.el")

;; Enable line numbers in programming modes.
(use-package display-line-numbers
  :hook ((prog-mode . display-line-numbers-mode)
         (conf-mode . display-line-numbers-mode)))

;; Show column numbers in the modeline.
(column-number-mode)

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

;; File name searches should be case-insensitive.
(setq read-file-name-completion-ignore-case t)

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
(setq kenran/holy-modes
      '((ediff . ediff)
        (eshell-mode . eshell)
        (mu4e . mu4e)
        (mu4e . mu4e-conversation)
        (notmuch-hello-mode . notmuch)
        (racket-repl-mode . nil)
        (racket-stepper-mode . nil)
        (shell-mode . nil)
        (sly-mrepl-mode . nil)
        (term-mode . (term term ansi-term multi-term))
        (vterm-mode . vterm)
        (haskell-interactive-mode . nil)))

(setq kenran/evil-holy-modes
      (mapcar #'car kenran/holy-modes))

(setq kenran/evil-collection-exemptions
      (remove nil
              (mapcar #'cdr kenran/holy-modes)))

;; The evil package offers a very complete vim experience inside of
;; Emacs.
(use-package evil
  :config
  (evil-mode 1)
  (dolist (mode kenran/evil-holy-modes)
    (evil-set-initial-state mode 'emacs))
  :custom
  ((evil-want-C-u-scroll t)
   (evil-want-C-u-delete nil)
   (evil-want-C-w-delete t)
   (evil-want-Y-yank-to-eol t)
   (evil-undo-system 'undo-redo)
   (evil-symbol-word-search t))
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (add-hook 'server-after-make-frame-hook
            #'kenran/set-evil-state-cursors))

;; This package makes it possible to enable evil-mode (and therefore
;; have a more vim-ish feel) in lots of (mostly minor) modes.  I'm not
;; sure whether I wish to use all of these (I think I don't need evil
;; in shells and REPLs), but I'll give them a try.
(use-package evil-collection
  :after evil
  :config
  (dolist (x kenran/evil-collection-exemptions)
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
  "b q" 'kill-current-buffer
  "b i" 'ibuffer
  "b k" 'kill-buffer
  "b n" 'next-buffer
  "b p" 'previous-buffer
  ;; Toggles/switches
  "t" '(:ignore t :which-key "toggle/switch")
  "t l" '(display-line-numbers-mode :which-key "line numbers")
  "t t" '(kenran/switch-theme :which-key "switch theme")
  "t r" '(kenran/reload-theme :which-key "reload theme")
  "t f" '(kenran/switch-font :which-key "switch font")
  "t w" 'whitespace-mode
  ;; Language-agnostic code-related commands
  "c" '(:ignore t :which-key "code")
  "c l" 'comment-line
  "c r" 'comment-or-uncomment-region
  ;; Searching
  "s" '(:ignore t :which-key "search/switch")
  "s g" 'consult-git-grep
  "s p" 'consult-ripgrep
  ;; Window management (redundant)
  "w" '(evil-window-map :which-key "windows")
  ;; Emacs config
  "e" '(:ignore t :which-key "emacs")
  "e e" '(kenran/open-init-file :which-key "edit init.el")
  "e c" '(kenran/open-other-config-file :which-key "edit other config file")
  "e t" '(kenran/open-custom-theme-file :which-key "edit custom theme"))

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

;; Enable C-w to delete backward (like in vim or bash) when Emacs is
;; reading user input in the minibuffer.
(general-define-key
 :keymaps 'minibuffer-local-map
 "C-w" 'evil-delete-backward-word)

(general-define-key
 :states '(emacs)
 "C-w" 'evil-delete-backward-word)

;;;; Package-specific configuration

;; The diminish package enables us to hide minor modes from the mode
;; line.  It's especially useful for certain modes that are globally
;; enabled anyway.  Use-package has built-in support for it available
;; with the :diminish keyword.
(use-package diminish)

;; Define and insert (mode-specific) snippets.
(use-package yasnippet
  :init (yas-global-mode 1)
  :diminish yas-minor-mode)

;; A terminal emulator for Emacs (more feature-rich than `term').
;; Currently just trying it out.
(use-package vterm
  :defer t
  :config
  (setq vterm-shell "fish")
  (general-define-key
   :keymaps 'vterm-mode-map
   :states 'emacs
   "C-w" 'vterm-send-C-w))

;; A mode for writing Nix expressions in.
(use-package nix-mode
  :mode "\\.nix\\'"
  :hook (before-save . nix-format-before-save))

;;; Markdown
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc")
  :hook ((markdown-mode gfm-mode) . auto-fill-mode))

;;; I use org-mode for lots of things now, but have only recently
;;; started doing so, hence my configuration is very much a work in
;;; progress.

;; Org-mode
(kenran/load-config-file "lisp/org.el")

;; Haskell
(kenran/load-config-file "lisp/haskell.el")

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

;;; Python
(use-package anaconda-mode
  :defer t
  :hook (python-mode . anaconda-mode))

(use-package pyimport
  :defer t)

;;; V
(use-package v-mode
  :defer t
  :mode ("\\.v\\'" . 'v-mode))

;;; Nim
(use-package nim-mode
  :defer t)

;; F#
(use-package fsharp-mode
  :defer t
  :config
  (setq fsharp-indent-offset 2)
  (setq fsharp-continuation-offset 2)
  (setq inferior-fsharp-program "dotnet fsi --readline-"))

;;; LSP integration

;; I've used lsp-mode in the past and while it's nice, I feel like
;; it's more in line with the rest of this configuration to try out
;; something more lightweight and closer to vanilla Emacs.  This is
;; where eglot comes into play.
(use-package eglot
  :defer t)

(use-package lsp-mode
  :defer t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp)

;; Mail configuration
(kenran/load-config-file "lisp/email.el")

;; This gives us better and more readable help pages.  We also replace
;; some built-in C-h keybings with helpful-* functions.
(use-package helpful
  :after evil
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key))
  :config
  (evil-set-initial-state 'helpful-mode 'motion))

(defun kenran/add-nix-envrc-file ()
  "If it doesn't already exist create a .envrc file containing 'use
nix' in the current directory."
  (interactive)
  (let ((envrc (expand-file-name ".envrc")))
    (if (file-exists-p envrc)
        (message "Envrc file already exists")
      (write-region "use nix" nil envrc))))

;;;###autoload
(defun kenran/project-vterm ()
  "Open a `vterm' session in the project root of the current
project.  Prompt if no project can be found."
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (vterm)))

(use-package project
  :config
  ;; Makes the `project-prefix-map' callable so that it can be bound
  ;; to a key with `with-leader'.
  (fset 'project-prefix-map project-prefix-map)
  (setq project-switch-commands
        '((project-find-file "Find file")
          (consult-ripgrep "Grep" ?g)
          (magit-status "Git status" ?v)
          (project-dired "Dired")
          (project-eshell "Eshell")
          (kenran/project-vterm "Vterm" ?t)))
  :bind (:map project-prefix-map
              ("t" . kenran/project-vterm)))

(with-leader
  "p" '(project-prefix-map :which-key "project"))

;;; Magit
(use-package magit
  :hook (git-commit-mode . evil-insert-state)
  :custom
  ;; No autosave for open buffers, as that might trigger hooks and
  ;; such.
  (magit-save-repository-buffers nil)
  (magit-diff-refine-hunk t)
  :config
  ;; I frequently pull with the autostash option.
  (transient-append-suffix 'magit-pull "-r"
    '("-a" "Autostash" "--autostash"))
  ;; ESC as alternative to C-g for going 'back' one transient level.
  (define-key transient-map [escape] #'transient-quit-one))

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

;; Browse the git history interactively.
(use-package git-timemachine
  :defer t)

;; Magit-specific keybindings are useful in a global scope, thus they
;; may be accessed under SPC g.
(with-leader
  "g" '(:ignore t :which-key "git")
  "g i" '(magit-gitignore :which-key "ignore")
  "g I" '(magit-init :which-key "init")
  "g s" '(magit-status :which-key "status")
  "g S" '(magit-status-here :which-key "status here")
  "g l" '(magit-log :which-key "log")
  "g f" '(magit-pull-from-upstream :which-key "pull")
  "g p" '(magit-push :which-key "pull")
  "g d" '(magit-diff :which-key "diff")
  "g t" '(magit-todos-list :which-key "todos"))

;; Show nice little VC annotations on the left side.
(use-package git-gutter
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode t))

;;; Smartparens
(use-package smartparens
  :diminish smartparens-mode
  :config
  (sp-pair "'" nil :actions nil)
  (sp-pair "`" nil :actions nil)
  (setq sp-highlight-pair-overlay nil)
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
        ("C-w" . evil-delete-backward-word)))

(use-package savehist
  :init
  (savehist-mode))

;;; Orderless

;; orderless is a completion style that fits in very well with
;; selectrum.  Parts of a search string may match according to several
;; matching styles.  We want to be able to specify which matching
;; style to use by appending a suffix so a search string.  Therefore
;; we define style dispatchers and use them to customize
;; orderless-style-dispatchers.

;; Prepending an equals sign to a search term will search for literal
;; matches of the preceding string.
(defun kenran/literal-if-= (pattern _index _total)
  (when (string-prefix-p "=" pattern)
    `(orderless-literal . ,(substring pattern 1))))

;; A prepended bang discards everything that matches the preceding
;; literal string.
(defun kenran/without-if-! (pattern _index _total)
  (when (string-prefix-p "!" pattern)
    `(orderless-without-literal . ,(substring pattern 1))))

;; The tilde sign gives me a way to have "fuzzy" search, if needed.
(defun kenran/flex-if-~ (pattern _index _total)
  (when (string-prefix-p "~" pattern)
    `(orderless-flex . ,(substring pattern 1))))

(use-package orderless
  :custom (completion-styles '(orderless))
  (orderless-style-dispatchers
   '(kenran/literal-if-=
     kenran/without-if-!
     kenran/flex-if-~)))

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
   (company-selection-wrap-around t))
  :bind
  (:map company-active-map
        ("C-w" . evil-delete-backward-word)))

(defun company--replacement-with-ws-face (str)
  "A workaround for company popups showing the space marks in
`whitespace-mode'.  See
https://github.com/company-mode/company-mode/issues/1231."
  (if (and (or global-whitespace-mode whitespace-mode)
           (memq 'space-mark whitespace-active-style)
           (memq 'face whitespace-active-style)
           (memq 'spaces whitespace-active-style))
      (let ((face `(:foreground ,(face-attribute 'whitespace-space :foreground))))
        (replace-regexp-in-string
         " "
         (lambda (s)
           (setq s (copy-sequence s))
           (add-face-text-property 0 (length s) face nil s)
           s)
         str))
    str))

(advice-add #'company--replacement-string
            :filter-return
            #'company--replacement-with-ws-face)

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
  :after hydra
  :config
  (setq default-text-scale-amount 15))

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
         (face-spec-set 'default `((t (:height ,kenran/default-font-height))))
         (set-face-attribute 'default nil
                             :height kenran/default-font-height))
   "reset" :color blue)
  ("q" nil "exit"))

(with-leader
  "s s" '(hydra-global-zoom/body :which-key "font zoom"))

;;; Built-in packages

(use-package whitespace
  :config
  (setq whitespace-style
        '(face spaces tabs trailing lines-tail space-before-tab
          indentation empty space-after-tab space-mark tab-mark
          missing-newline-at-eof))
  (setq whitespace-line-column 100)
  (setq whitespace-global-modes
        '(not magit-status-mode))
  (global-whitespace-mode 1))

;; Render manpages in Emacs
(use-package man
  :defer t
  :config
  ;; As soon as it is ready open the manpage in a separate, focused
  ;; window.
  (setq Man-notify-method 'aggressive))

;; Diminish only.
(use-package face-remap
  :ensure nil
  :diminish buffer-face-mode)

;; Diminish only.
(use-package autorevert
  :ensure nil
  :diminish auto-revert-mode)

(use-package simple
  :ensure nil
  :diminish auto-fill-function
  :config
  (setq eval-expression-print-length nil)
  (setq eval-expression-print-level nil))

;; Diminish only.
(use-package eldoc
  :ensure nil
  :diminish eldoc-mode
  :config
  (advice-add 'eldoc-doc-buffer
              :after
              (defun kenran/focus-eldoc-buffer ()
                (message (buffer-name (current-buffer)))
                (pop-to-buffer eldoc--doc-buffer))))

(use-package dired
  :ensure nil
  :defer t
  :config
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq dired-create-destination-dirs 'ask)
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
