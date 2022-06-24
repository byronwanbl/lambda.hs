module Lambda.Lambda (Identifier (..), Expr (..), simplify, apply, replace) where

newtype Identifier = Identifier String deriving (Eq, Show)

data Expr
  = Id Identifier
  | Func Identifier Expr
  | Apply Expr Expr
  deriving (Eq, Show)

simplify :: Expr -> Expr
simplify expr@(Id _) = expr
simplify (Func id expr) = Func id (simplify expr)
simplify (Apply func expr) = apply (simplify func) (simplify expr)

apply :: Expr -> Expr -> Expr
apply (Func v b) expr = replace v expr b
apply _ _ = error "Can only `apply` a function"

replace :: Identifier -> Expr -> Expr -> Expr -- old, new, src -> src[old := new]
replace old new (Id id)
  | id == old = new
  | otherwise = Id id
replace old new (Func id expr)
  | id == old = Func id expr
  | otherwise = Func id (replace old new expr)
replace old new (Apply func expr) = Apply (replace old new func) (replace old new expr)
