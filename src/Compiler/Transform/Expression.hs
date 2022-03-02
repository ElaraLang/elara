module Compiler.Transform.Expression where

import Compiler.Transform.Abstract
import Compiler.Transform.Environment
import Compiler.Transform.Types
import Control.Monad (when)
import Control.Monad.State.Lazy (get, gets, modify)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Interpreter.AST as E

compileConstant :: E.Constant -> (JVMType, ConstantPoolEntry)
compileConstant (E.IntC i) = do
  let converted = fromInteger i :: Int -- TODO overflow but who cares
  (JVMInt, CPIntegerEntry converted)
compileConstant (E.StringC s) = do
  let converted = T.pack s
  (JVMObject "java/lang/String", CPStringEntry converted)

compileExpression :: Compiler E.Expression ()
compileExpression (E.Bind (E.IdentifierPattern ident) (E.Constant constant)) = do
  state <- get
  let clazz = classFile state
  let (jvmType, compiled) = compileConstant constant
  let expectedType = fromMaybe jvmType (M.lookup (show ident) (expectedTypes state))

  when (expectedType /= jvmType) $
    error $ "Expected type " ++ show expectedType ++ " for variable " ++ show ident ++ " but got " ++ show jvmType

  let field =
        Field
          { accessFlags = [ACC_PUBLIC, ACC_STATIC, ACC_FINAL],
            name = T.pack $ show ident,
            descriptor = T.pack $ toInternalType expectedType,
            attributes =
              [ ConstantValueAttribute compiled
              ]
          }
  modify (\s -> s {classFile = clazz {fields = field : fields clazz}})

compileDefLine :: Compiler (E.Identifier, E.Type) ()
compileDefLine (pattern, t) = do
  types <- gets expectedTypes
  let expectedType = elaraTypeToJVMType t
  let varName = show pattern
  case M.lookup varName types of
    Just _ -> error $ "Duplicate def pattern " ++ show pattern ++ " : " ++ show t
    Nothing -> modify (\s -> s {expectedTypes = M.insert varName expectedType types})

compileLine :: Compiler E.Line ()
compileLine (E.ExpressionLine e) = compileExpression e
compileLine (E.DefLine pattern t) = compileDefLine (pattern, t)

compileLines :: Compiler [E.Line] ClassFile
compileLines lines = do
  mapM_ compileLine lines
  gets classFile
