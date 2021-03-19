;; TODO:
;; - orderless?
;; - consult
;; - embark??
;; - leader key
;; - org!!
;; - more evil stuff: https://github.com/hlissner/doom-emacs/blob/develop/modules/editor/evil/packages.el
;; - projectile
;; - company
;; - which-key
;; - mu4e
;; - ibuffer
;; - dired+
;; - lsp
;; - ripgrep
;; - smartparens/paredit/...
;; - magit-todos
;; - pass
;; - doom-themes
;; - doom-modeline
;; - windows/avy
;; - format on save
;; - auto-close parens (smartparens)

;; languages:
;; - haskell
;; - data formats
;; - haskell-mode

;; Put =customize= code into a separate file.
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'no-error)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(load-theme 'modus-vivendi t)

;; 'y' or 'n' should always suffice.
(fset 'yes-or-no-p 'y-or-n-p)

;; Use general to define keymaps.
(use-package general)

;; Use SPC as leader key, like in Doom and neovim.
(general-create-definer my-leader-def
  :prefix "SPC")

;; Might use something else like , later, but SPC m should
;; suffice for now.
(general-create-definer my-local-leader-def
  :prefix "SPC m")

(use-package evil
  :config
  (evil-mode 1)
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (general-define-key
   :states 'normal
   :keymap 'evil-window-map
   ;; Normal mode keybindings for evil commands
   "C-w C-h" 'evil-window-left
   "C-w C-j" 'evil-window-down
   "C-w C-k" 'evil-window-up
   "C-w C-l" 'evil-window-right
   "C-w d" 'evil-window-delete
   "C-w C-d" 'evil-window-delete)
  (my-leader-def
    :keymaps 'normal
    "b q"))

(use-package projectile
  :config
  (projectile-mode +1))

;; FIXME: define keys in :init sections

(my-leader-def
  :keymaps 'normal
  ;; SPC SPC as M-x alias
  "SPC" 'execute-extended-command
  ;; Buffer commands
  "b b" 'ibuffer
  ;; Projectile commands
  "p f" 'projectile-find-file
  "p p" 'projectile-switch-project)

(use-package evil-collection
  :after evil
  :config
  ;; Register all bindings in evil-collection
  ;(evil-collection-init 'apropos) ;; TODO
  ;(evil-collection-init 'cider) ;; TODO
  ;(evil-collection-init 'company) ;; TODO check
  (evil-collection-init 'dired) ;; TODO check
  ;(evil-collection-init 'dired-sidebar) ;; TODO check
  ;(evil-collection-init 'ediff) ;; TODO
  (evil-collection-init 'elisp-mode)
  ;(evil-collection-init 'flycheck) ;; TODO
  ;(evil-collection-init 'ibuffer) ;; TODO
  ;(evil-collection-init 'ivy) ;; TODO
  (evil-collection-init 'magit)
  ;(evil-collection-init 'magit-todos) ;; TODO
  ;(evil-collection-init 'pass)
  ;(evil-collection-init 'ripgrep)
  ;(evil-collection-init 'which-key) ;; TODO
  )

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package selectrum
  :config
  (selectrum-mode +1))

(use-package smartparens
  :config
  (smartparens-global-mode 1))

(use-package orderless
  ;; TODO customize?
  :custom (completion-styles '(orderless)))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(use-package hl-todo
  ;; TODO customize keybindings (previous/next/...)
  :init
  (add-hook 'after-init-hook 'global-hl-todo-mode))
