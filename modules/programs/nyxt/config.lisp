(setf (uiop/os:getenv "WEBKIT_DISABLE_COMPOSITING_MODE")
      "1")

;; Copied from aartaka's config, as I'm using the current master
;; branch build and the documentation is not up to date yet.
(define-configuration (web-buffer prompt-buffer nyxt/editor-mode:editor-buffer)
    ((default-modes `(nyxt/emacs-mode:emacs-mode ,@%slot-value%))))

(define-configuration web-buffer
    ((current-zoom-ratio 1.25)))

(define-configuration browser
    ((theme (make-instance
             'theme:theme
             :dark-p t
             :background-color "black"
             :on-background-color "white"
             :accent-color "#CD5C5C"
             :primary-color "rgb(170, 170, 170)"
             :on-primary-color "black"
             :secondary-color "rgb(100, 100, 100)"
             :on-secondary-color "white"))))
