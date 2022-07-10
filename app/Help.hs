{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Help (help) where

import Text.RawString.QQ ( r )

help :: String
help = intro ++ logo ++ usage

intro :: String
intro =
  [r|
lambda.hs, Lambda calculus calculator in haskell.
Copyright (C) 2022-present ByronWan<byronwan@outlook.com>.
|]

logo :: String
logo =
  [r|
 _                 _         _         _
| | __ _ _ __ ___ | |__   __| | __ _  | |__  ___
| |/ _` | '_ ` _ \| '_ \ / _` |/ _` | | '_ \/ __|
| | (_| | | | | | | |_) | (_| | (_| |_| | | \__ \
|_|\__,_|_| |_| |_|_.__/ \__,_|\__,_(_)_| |_|___/
|]

usage :: String
usage = 
  [r|
Read <baike.baidu.com/item/%CE%BB%E6%BC%94%E7%AE%97> or <zh.wikipedia.org/%CE%BB%E6%BC%94%E7%AE%97>
to know more about **Lambda calculus**.

Usage:
  `<expression>`         Calculate (simplify) the expression.
  `<var> = <expression>` Set the variable to expression (will not simplify).

  `:help`         Show this message.
  `:list`         List all variables.       
  `:save <file>`  Save data to specific file. (WIP)
  `:load <file>`  Load data to specific file. (WIP)
  `:quit` or `:q` Quit the program.
  |]