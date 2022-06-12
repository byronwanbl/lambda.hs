module Lambda.Show (showExpr) where

import Data.Function ((&))
import Data.Functor ((<&>))
import Lambda.Lambda (Expr (..), Identifier (..))

showId :: Identifier -> String
showId (Identifier id)
  | length id == 1 = id
  | otherwise = "`" <> id <> "`"

showExpr :: Expr -> String
showExpr (Id id@(Identifier _)) = showId id
showExpr func@(Func _ _) = "(λ" <> (args <&> showId & unwords) <> "." <> showExpr expr <> ")"
  where
    fold :: Expr -> [Identifier] -> ([Identifier], Expr)
    fold (Func id expr) arg = fold expr (arg <> [id])
    fold x arg = (arg, x)
    (args, expr) = fold func []
showExpr x@(Apply _ _) = "(" <> (fold x <&> showExpr & unwords) <> ")"
  where
    fold :: Expr -> [Expr]
    fold (Apply func expr) = fold func <> [expr]
    fold x = [x]
