(use-package haskell-mode
  :diminish interactive-haskell-mode
  :custom
  (haskell-process-type 'cabal-repl)
  (haskell-interactive-popup-errors nil)
  :hook (haskell-mode . interactive-haskell-mode))

;; A couple of Emacs Lisp functions that help me to make quick changes
;; to Haskell files (adding pragmas, language extensions, GHC
;; options).

(defun kenran/make-pragma (pragma content)
  "Create a pragma line of type `pragma' containing `content'."
  (concat "{-# " pragma " " content " #-}\n"))

(defun kenran/haskell-add-language-extension (ext-name)
  "Add an extension from the list of available language extensions
to the top of the file."
  (interactive
   (list
    (completing-read
     "Extension: "
     haskell-ghc-supported-extensions)))
  (let ((pragma (kenran/make-pragma "LANGUAGE" ext-name)))
    (save-excursion
      (goto-char (point-min))
      (insert pragma))))

(defun kenran/haskell-add-ghc-option (opt-name)
  "Add a GHC option from the list of options to the top of the
file."
  (interactive
   (list
    (completing-read
     "GHC option: "
     haskell-ghc-supported-options)))
  (let ((pragma (kenran/make-pragma "OPTIONS_GHC" opt-name)))
    (save-excursion
      (goto-char (point-min))
      (insert pragma))))

(defun kenran/read-non-empty-string (prompt)
  "Read a string from the minibuffer.  When the result is the empty
string, return nil instead."
  (let ((str (read-string prompt)))
    (unless (string-empty-p str)
      str)))

;; This function is the one is use the most (by far).  It makes it
;; somewhat easy to add =import= statements to Haskell files.  It's
;; surely not perfect, but fits my preferences well; that is, I almost
;; never use the combinations that are harder to add with this
;; template.  For instance, adding a qualified import with import
;; list, or an unqualified one with an alias, would require "tricks".

(defun kenran/haskell-add-import (module &optional qualified? alias)
  "Add an import to the import list.  Prompts for qualified import
and alias."
  (interactive
   (let* ((module (read-string "Module: "))
          (qualified?
           (unless (string-match-p "(" module)
             (y-or-n-p (concat "Import " module " qualified?"))))
          (alias (when qualified?
                   (kenran/read-non-empty-string "Alias [or leave empty]: "))))
     (list module qualified? alias)))
  (let ((import-line
         (concat "import "
                 (when qualified? "qualified ")
                 module
                 (when alias (concat " as " alias))
                 "\n")))
    (save-excursion
      (haskell-navigate-imports-go)
      (insert import-line))))

;; Define some keybindings that are local to the
;; =interactive-haskell-mode= using the local leader key.

(with-eval-after-load 'haskell
  (keymap-set interactive-haskell-mode-map "C-c e f" 'haskell-goto-first-error)
  (keymap-set interactive-haskell-mode-map "C-c e n" 'haskell-goto-next-error)
  (keymap-set interactive-haskell-mode-map "C-c e p" 'haskell-goto-prev-error)
  (keymap-set interactive-haskell-mode-map "C-c i" 'kenran/haskell-add-import)
  (keymap-set interactive-haskell-mode-map "C-c p l" 'kenran/haskell-add-language-extension)
  (keymap-set interactive-haskell-mode-map "C-c p o" 'kenran/haskell-add-ghc-option)
  (keymap-set interactive-haskell-mode-map "C-c h k" 'haskell-session-kill)
  (keymap-set interactive-haskell-mode-map "C-c h r" 'haskell-process-restart))

(provide 'my-haskell)
