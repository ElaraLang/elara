{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc,
  )
where

import Compiler.Instruction
import Compiler.Transform.Abstract
import Compiler.Transform.Expression
import Compiler.Transform.Transform
import Control.Monad (forM_)
import Data.Binary.Put
import Data.Bits ((.|.))
import Data.ByteString.Lazy as L (unpack, writeFile)
import Interpreter.AST
import Interpreter.Execute
import Parse.Parser
import Parse.Reader
import Parse.Utils
import Preprocess.Preprocessor
import qualified Compiler.ClassFile as C

someFunc :: IO ()
someFunc = do
  content <- readFile "source.elr"
  print content
  let tokens = evalP readTokens content
  putStrLn "Tokens: "
  print tokens
  let ast = parse content
  env <- initialEnvironment
  
  let emptyClass = ClassFile {className = "Test", superName = "", fields = []}
  
  let compiled = foldl (\clazz (ExpressionLine e) -> compileExpression e clazz) emptyClass $ preprocess <$>  ast
  let classFile = transform compiled
  L.writeFile "Test.class" (runPut $ C.putClassFile classFile)