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
import Preprocess.Preprocessor

someFunc :: IO ()
someFunc = do
  content <- readFile "source.elr"
  print content
  let tokens = evalP readTokens content
  putStrLn "Tokens: "
--  print tokens
  let ast = parse content
  env <- initialEnvironment
  forM_ ast $ \line -> do
--    print line
    let preprocessed = preprocess line
--    putStrLn "Preprocessed AST: "
--    print preprocessed
    execute preprocessed env
