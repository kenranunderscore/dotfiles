;; Provide an interactive mode for writing Haskell.  I can work with a
;; REPL, get feedback and compilation errors shown in the code, and so
;; on.
(use-package haskell-mode
  :diminish interactive-haskell-mode
  :custom
  (haskell-process-type 'cabal-repl)
  :hook (haskell-mode . interactive-haskell-mode))

;;;###autoload
(defun my--make-pragma (pragma content)
  "Create a pragma line of type `pragma' containing `content'."
  (concat "{-# " pragma " " content " #-}\n"))

;;;###autoload
(defun my--haskell-add-language-extension (ext-name)
  "Add an extension from the list of available language extensions
to the top of the file."
  (interactive
   (list
    (completing-read
     "Extension: "
     haskell-ghc-supported-extensions)))
  (let ((pragma (my--make-pragma "LANGUAGE" ext-name)))
    (save-excursion
      (goto-char (point-min))
      (insert pragma))))

;;;###autoload
(defun my--haskell-add-ghc-option (opt-name)
  "Add a GHC option from the list of options to the top of the
file."
  (interactive
   (list
    (completing-read
     "GHC option: "
     haskell-ghc-supported-options)))
  (let ((pragma (my--make-pragma "OPTIONS_GHC" opt-name)))
    (save-excursion
      (goto-char (point-min))
      (insert pragma))))

(defun my--read-non-empty-string (prompt)
  "Read a string from the minibuffer.  When the result is the empty
string, return nil instead."
  (let ((str (read-string prompt)))
    (unless (string-empty-p str)
      str)))

;;;###autoload
(defun my--haskell-add-import (module &optional qualified? alias)
  "Add an import to the import list.  Prompts for qualified import
and alias."
  (interactive
   (let* ((module (read-string "Module: "))
          (qualified? (y-or-n-p (concat "Import " module " qualified?")))
          (alias (when qualified?
                   (my--read-non-empty-string "Alias [or leave empty]: "))))
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
;; interactive-haskell-mode using the local leader key.
(with-local-leader
  :states 'normal
  :keymaps 'interactive-haskell-mode-map
  "e" '(:ignore t :which-key "errors")
  "e f" '(haskell-goto-first-error :which-key "first")
  "e n" '(haskell-goto-next-error :which-key "next")
  "e p" '(haskell-goto-prev-error :which-key "previous")
  "i" '(:ignore t :which-key "imports")
  "i i" '(haskell-navigate-imports-go :which-key "navigate to imports")
  "i r" '(haskell-navigate-imports-return :which-key "return from imports")
  "i a" '(my--haskell-add-import :which-key "add import")
  "p" '(:ignore t :which-key "pragmas")
  "p l" '(my--haskell-add-language-extension :which-key "add language extension")
  "p o" '(my--haskell-add-ghc-option :which-key "add GHC option"))
