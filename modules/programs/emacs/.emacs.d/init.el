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

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

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
