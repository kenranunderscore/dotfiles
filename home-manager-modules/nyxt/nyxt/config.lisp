(setf (uiop/os:getenv "WEBKIT_DISABLE_COMPOSITING_MODE")
      "1")

;; Use Emacs mode everywhere
(define-configuration buffer
  ((default-modes
    (pushnew 'nyxt/mode/emacs:emacs-mode %slot-value%))))

(define-configuration (buffer web-buffer)
  ((zoom-ratio-default 1.5)))

;; TODO: colors need much tweaking
(define-configuration browser
    ((theme (make-instance
             'theme:theme
             :background-color "#040404"
             :on-background-color "#0ac30a"
             :accent-color "#01018a"
             :on-accent-color "#0ac30a"
             :primary-color "rgb(170, 170, 170)"
             :on-primary-color "black"
             :secondary-color "rgb(100, 100, 100)"
             :on-secondary-color "white"))))
