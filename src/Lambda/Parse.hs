module Lambda.Parse (parseRoot, parseMain) where

import Data.Function ((&))
import Data.Functor ((<&>))
import Lambda.Lambda (Expr (..), Identifier (..))
import Text.ParserCombinators.Parsec (Parser, alphaNum, char, letter, many1, parse, satisfy, skipMany1, space, spaces, try, (<|>))
import Text.ParserCombinators.Parsec.Combinator (choice, eof)

parseRoot :: Parser Expr
parseRoot = parseApply

parseId :: Parser Identifier
parseId =
  try
    ( do
        char '`'
        i <- many1 alphaNum
        char '`'
        return (Identifier i)
    )
    <|> Identifier . (: [])
    <$> alphaNum

parseFunc :: Parser Expr
parseFunc = do
  char 'λ'
  spaces
  param <- many1 parseId
  spaces
  char '.'
  expr <- parseRoot
  return (foldr Func expr param)

parseAtom :: Parser Expr
parseAtom =
  try
    ( do
        char '('
        spaces
        expr <- parseRoot
        spaces
        char ')'
        return expr
    )
    <|> choice [parseFunc, parseId <&> Id]

parseApply :: Parser Expr
parseApply =
  try
    ( do
        func <- parseAtom
        spaces
        a <- many1 parseAtom
        return (foldl Apply func a)
    )
    <|> parseAtom

parseMain :: String -> Expr
parseMain s = parse param "" s & either (\expr -> error ("Parse failed: " <> show expr)) id
  where
    param = do
      expr <- parseRoot
      eof
      return expr
