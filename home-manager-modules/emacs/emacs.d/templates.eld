fundamental-mode

(fm "FIXME(Johannes): ")
(td "TODO(Johannes): ")

lisp-mode emacs-lisp-mode

(lambda "(lambda (" p ")" n> r> ")")

emacs-lisp-mode

(fun "(defun " p " (" p ")\n  \"" p "\"" n> r> ")")

haskell-mode

(main "main :: IO ()" n "main = do" n> p)
(mod "module " p " where" n n)
(gadt "data " p " where" n> p " :: ")
(prag "{-# " p "#-}")
(rec "data " (s name) " = " name n>
     "{ " p " :: " p " }")
(fun (s name) " :: " p n name " " p " = ")
(ds "deriving (Eq, Show)")
(dsg "deriving (Eq, Show, Generic)")
(impl "(error \"TODO: " (p "not implemented") "\")")
(st "data " p n> "= " p)
(toj "instance Aeson.ToJSON " p " where" n> "toJSON " p)
(fromj "instance Aeson.FromJSON " p " where" n> "parseJSON " p)
