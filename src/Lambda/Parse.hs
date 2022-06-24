module Lambda.Parse (parseExpr, parseId) where

import Data.Function ((&))
import Data.Functor ((<&>))
import Lambda.Lambda (Expr (..), Identifier (..))
import Text.ParserCombinators.Parsec (Parser, alphaNum, char, letter, many1, parse, satisfy, skipMany1, space, spaces, try, (<|>))
import Text.ParserCombinators.Parsec.Combinator (choice, eof)

parseExpr :: Parser Expr
parseExpr = parseApply

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
  expr <- parseExpr
  return (foldr Func expr param)

parseAtom :: Parser Expr
parseAtom =
  try
    ( do
        char '('
        spaces
        expr <- parseExpr
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
