module Lib
  ( someFunc,
  )
where

import Lexer
import Parser

someFunc :: IO ()
someFunc = do
  content <- readFile "source.elr"

  let toks = alexScanTokens content
  let ast = parseElara toks
  print ast
--  print $ parseElara content
