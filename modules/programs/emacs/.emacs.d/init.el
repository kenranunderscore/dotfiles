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

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
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
  :ensure t
  :config
  (selectrum-mode +1))

(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode 1))

(use-package orderless
  :ensure t
  ;; TODO customize?
  :custom (completion-styles '(orderless)))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))
