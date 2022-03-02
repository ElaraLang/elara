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
import Interpreter.AST
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
  print tokens
  let ast = parse content

  let emptyClass = ClassFile {className = "Test", superName = "", fields = []}

  let compiled = foldl (\clazz (ExpressionLine e) -> compileExpression e clazz) emptyClass $ preprocess <$> ast
  let classFile = transform compiled
  L.writeFile "Test.class" (runPut $ C.putClassFile classFile)
