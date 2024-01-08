module Pretty where

import Zilly

------------------------------
-- pretty printers           |
------------------------------

mkParens :: Bool -> String -> String
mkParens b s = if b then "(" <> s <> ")" else s

prettyType :: T -> String
prettyType = prettyType' ((-1) :: Float)
  where 
    prettyType' _ Z = "Z"
    prettyType' pPrec (Lazy y) = mkParens (pPrec > 3) $ "lazy " <> prettyType' 3 y 
    prettyType' pPrec (Fun left@(Fun _ _) right) 
      = mkParens (pPrec > 2) $ prettyType' 2.1 left <> " -> " <> prettyType' 2 right
    prettyType' pPrec (Fun left right) 
      = mkParens (pPrec > 2) $ prettyType' 2 left <> " -> " <> prettyType' 2 right

prettyExp :: E -> String
prettyExp = prettyExp'
  where
    -- no parent precedence argument since it's just easier to paren by hand
    -- parent decides if child is paren
    prettyExp' (Val n)        = show n
    prettyExp' (Sym v)        = v
    prettyExp' (Minus x y)    = "minus" <> mkParens True (prettyExp' x) <> mkParens True (prettyExp' y)
    prettyExp' (Less x y)     = "lt" <> mkParens True (prettyExp' x) <> mkParens True (prettyExp' y)
    prettyExp' (If (b,x,y))   = "if" <> mkParens True (prettyExp' b <> ", " <> prettyExp' x <> ", " <> prettyExp' y)
    prettyExp' (Defer e)      = "'" <> prettyExp' e <> "'"
    prettyExp' (Apply(Sym f) e)        = f <> mkParens True (prettyExp' e)
    prettyExp' (Apply(Defer f) e)      = prettyExp' f <> mkParens True (prettyExp' e)
    prettyExp' (Apply f@(Apply _ _) e) = prettyExp' f <> mkParens True (prettyExp' e)
    prettyExp' (Apply f e)    = mkParens True (prettyExp' f) <> mkParens True (prettyExp' e)
    prettyExp' (Lambda t v e) =  "<" <> prettyType t <> "> " <> prettyExp' v <> " -> " <> prettyExp' e 
