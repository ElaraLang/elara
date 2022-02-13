module Lib
  ( someFunc,
  )
where

import Control.Monad (forM_)
import Data.List (intercalate)
import Interpreter.Execute
import Parse.AST
import Parse.Parser
import Parse.Reader
import Parse.Utils

someFunc :: IO ()
someFunc = do
  content <- readFile "source.elr"
  let tokens = evalP readTokens content
  print tokens
  let ast = parse content
  env <- initialEnvironment
  forM_ ast $ \line ->
    execute line env




