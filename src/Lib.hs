{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc,
  )
where

import qualified Compiler.ClassFile as C
import Compiler.Transform.Abstract
import Compiler.Transform.Environment (emptyCompileState)
import Compiler.Transform.Expression
import Compiler.Transform.Transform
import Control.Monad.State.Lazy (evalState)
import Data.Binary.Put
import Data.ByteString.Lazy as L (writeFile)
import Parse.Parser
import Preprocess.Preprocessor

someFunc :: IO ()
someFunc = do
  content <- readFile "source.elr"
  let ast = parse content

  let emptyClass = ClassFile {className = "Test", superName = "java/lang/Object", fields = []}
  let compileState = emptyCompileState emptyClass

  let preprocessed = preprocess <$> ast
  let compiled = evalState (compileLines preprocessed) compileState
  let classFile = transform compiled
  L.writeFile "Test.class" (runPut $ C.putClassFile classFile)
