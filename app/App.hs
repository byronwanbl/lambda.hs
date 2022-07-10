{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module App where

import qualified Control.Monad
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.HashMap.Internal.Strict (HashMap, empty, foldlWithKey', insert, keys)
import Data.Hashable (Hashable (hashWithSalt))
import Help (help)
import Lambda (Expr, Identifier (..), parseExpr, parseId, replace, showExpr, showId, simplify)
import System.Console.Isocline (Completion (Completion), CompletionEnv, completeQuotedWord, completeWord, completion, completionsFor, enableAutoTab, historyAdd, putFmtLn, readlineEx, setHistory, wordCompleter)
import System.Exit (exitSuccess)
import Text.ParserCombinators.Parsec (Parser, choice, eof, parse, spaces, string, try, (<|>))

app :: IO ()
app =
  do
    putFmtLn help
    setHistory "/tmp/lambda.hs.history" 2000
    loop mainLoop (State empty)

mainLoop :: Proc
mainLoop s = do
  cmd <- readlineEx "λ" (Just (completer s)) Nothing
  let r = parse rootParser "" cmd
  historyAdd cmd
  either
    ( \err -> do
        putFmtLn "Parse command failed: "
        putFmtLn (show err)
        return s
    )
    ( \p -> do
        p s
    )
    r

data State = State
  { variableSpace :: HashMap Identifier Expr
  }

rootParser :: Parser Proc
rootParser =
  choice
    [ try
        ( do
            try (string ":quit") <|> string ":q"
            spaces
            return (const exitSuccess)
        ),
      try
        ( do
            eof
            return return
        ),
      try
        ( do
            string ":help"
            spaces
            return
              ( \x -> do
                  putFmtLn help
                  return x
              )
        ),
      try procList,
      try procSetVariable,
      try procCalc
    ]

procSetVariable :: Parser Proc
procSetVariable = do
  var <- parseId
  spaces
  string "="
  spaces
  expr <- parseExpr
  return
    ( \s ->
        return State {variableSpace = insert var expr (variableSpace s)}
    )

procCalc :: Parser Proc
procCalc = do
  expr <- parseExpr
  return
    ( \s -> do
        let expr1 = foldlWithKey' (\src old new -> replace old new src) expr (variableSpace s)
        case simplify expr1 of
          Nothing -> putFmtLn "Simplify failed."
          Just expr2 -> putFmtLn (showExpr expr2)
        return s
    )

procList :: Parser Proc
procList = do
  string ":list"
  return
    ( \st -> do
        putFmtLn $ variableSpace st & foldlWithKey' (\s k v -> s ++ showId k ++ " = " ++ showExpr v ++ "\n") ""
        return st
    )

type Proc = State -> IO State

completer :: State -> CompletionEnv -> String -> IO ()
completer s env str = do
  wordCompleter (variableSpace s & keys <&> showId) env str
  wordCompleter ["λ"] env str
  Control.Monad.when (null str) $ wordCompleter [":list", ":help", ":quit"] env str

loop :: Monad m => (a -> m a) -> a -> m ()
loop f x = do
  y <- f x
  loop f y

instance Hashable Identifier where
  hashWithSalt salt (Identifier x) = hashWithSalt salt x