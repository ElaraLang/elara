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
import Text.Pretty.Simple
import TypeInfer.Env
import TypeInfer.Infer

someFunc :: IO ()
someFunc = do
  content <- readFile "source.elr"
  let ast = parse content
  printColored ast
  let emptyClass = ClassFile {className = "Test", superName = "java/lang/Object", fields = []}
  let compileState = emptyCompileState emptyClass

  let preprocessed = preprocessAll ast
  printColored preprocessed
  --  let compiled = evalState (compileLines preprocessed) compileState
  --  let classFile = transform compiled
  --  L.writeFile "Test.class" (runPut $ C.putClassFile classFile)

  putStrLn "Type Inference: "
  printColored (inferLines preprocessed baseEnv)

printColored :: (Show a) => a -> IO ()
printColored = pPrintOpt NoCheckColorTty defaultOutputOptionsDarkBg
