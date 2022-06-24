module Lambda
  ( Expr (..),
    Identifier (..),
    simplify,
    apply,
    replace,
    showExpr,
    showId,
    parseExpr,
    parseId,
  )
where

import Lambda.Lambda
  ( Expr (..),
    Identifier (..),
    apply,
    replace,
    simplify,
  )
import Lambda.Parse (parseExpr, parseId)
import Lambda.Show (showExpr, showId)
