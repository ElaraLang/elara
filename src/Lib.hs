module Lib
  ( someFunc,
  )
where

import Parse.File

someFunc :: IO ()
someFunc = do
  content <- readFile "source.elr"
  print $ parseElara content
