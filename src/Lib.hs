{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc,
  )
where

import Compiler.Transform.Abstract
import Compiler.Transform.Environment (emptyCompileState)
import qualified Data.Map as M
import qualified Interpreter.AST as A
import Parse.Parser
import Preprocess.Preprocessor
import TypeInfer.Env
import TypeInfer.Infer
import Text.Pretty.Simple (pPrint)

someFunc :: IO ()
someFunc = do
  content <- readFile "source.elr"
  let ast = parse content
  pPrint ast
  let emptyClass = ClassFile {className = "Test", superName = "java/lang/Object", fields = []}
  let compileState = emptyCompileState emptyClass
 
  let preprocessed = preprocessAll ast
  pPrint preprocessed
  --  let compiled = evalState (compileLines preprocessed) compileState
  --  let classFile = transform compiled
  --  L.writeFile "Test.class" (runPut $ C.putClassFile classFile)

  putStrLn "Type Inference: "
  pPrint (inferLines preprocessed baseEnv)
