(in-package :stumpwm)

(ql:quickload :slynk)
(defparameter *slynk-port* 4444)

;; Start slynk in a background thread
(sb-thread:make-thread
 (lambda ()
   (slynk:create-server
    :port *slynk-port*
    :dont-close t))
 :name "slynk-auto")

(load-module "ttf-fonts")
(setq clx-truetype::*font-dirs*
      (append (list (namestring "~/.nix-profile/share/fonts")
                    (namestring "~/.nix-profile/share/fonts/truetype")
                    (namestring "~/.nix-profile/share/fonts/opentype"))
              clx-truetype::*font-dirs*))
(when (not (find "JetBrains Mono Regular" (xft:get-font-families)
                 :test #'equal))
  (xft:cache-fonts))

(set-font (make-instance 'xft:font
                         :family "JetBrains Mono"
                         :subfamily "Regular"
                         :size 20
                         :antialias t))

(setf *message-window-gravity* :center
      *message-window-padding* 5)
(setf *input-window-gravity* :center)
