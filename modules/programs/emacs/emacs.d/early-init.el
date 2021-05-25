;; Emacs startup can be significantly sped up by reducing the number
;; of garbage collections that take place during initialization.  The
;; default `gc-cons-threshold' of 80 kilobytes is way too low for any
;; more recent system.  Still it's beneficial to reset this temporary
;; value back to a lower number after initialization.  That way the GC
;; pause won't be as long when working within Emacs.  `gcmh-mode' will
;; be started at the end of my init file, which will then control the
;; GC behavior.
(setq gc-cons-threshold most-positive-fixnum)
