;; TODO:
;; - leader key
;; - org!!
;; - ivy / selectrum
;; - projectile
;; - company
;; - which-key
;; - mu4e
;; - ibuffer
;; - dired+
;; - lsp
;; - haskell-mode
;; - ripgrep
;; - smartparens/paredit/...
;; - magit-todos
;; - pass
;; - doom-themes
;; - doom-modeline
;; - windows/avy

(setq evil-want-keybinding nil)

;; Enable evil-mode
(evil-mode 1)

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
