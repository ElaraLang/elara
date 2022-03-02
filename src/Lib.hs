{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc,
  )
where

import qualified Compiler.ClassFile as C
import Compiler.Transform.Abstract
import Compiler.Transform.Expression
import Compiler.Transform.Transform
import Data.Binary.Put
import Data.ByteString.Lazy as L (writeFile)
import Parse.Parser
import Preprocess.Preprocessor

someFunc :: IO ()
someFunc = do
  content <- readFile "source.elr"
  let ast = parse content

  let emptyClass = ClassFile {className = "Test", superName = "java/lang/Object", fields = []}

  let compiled = foldl (flip compileLine) emptyClass $ preprocess <$> ast
  let classFile = transform compiled
  L.writeFile "Test.class" (runPut $ C.putClassFile classFile)
