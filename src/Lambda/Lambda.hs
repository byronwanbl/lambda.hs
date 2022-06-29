module Lambda.Lambda (Identifier (..), Expr (..), simplify, apply, replace) where

newtype Identifier = Identifier String deriving (Eq, Show)

data Expr
  = Id Identifier
  | Func Identifier Expr
  | Apply Expr Expr
  deriving (Eq, Show)

simplify :: Expr -> Maybe Expr
simplify expr@(Id _) = Just expr
simplify (Func id expr) = do
  expr' <- simplify expr
  Just (Func id expr)
simplify (Apply func expr) = do
  func' <- simplify func
  expr' <- simplify expr
  apply func' expr'

apply :: Expr -> Expr -> Maybe Expr
apply (Func v b) expr = Just (replace v expr b)
apply _ _ = Nothing

replace :: Identifier -> Expr -> Expr -> Expr -- old, new, src -> src[old := new]
replace old new (Id id)
  | id == old = new
  | otherwise = Id id
replace old new (Func id expr)
  | id == old = Func id expr
  | otherwise = Func id (replace old new expr)
replace old new (Apply func expr) = Apply (replace old new func) (replace old new expr)
