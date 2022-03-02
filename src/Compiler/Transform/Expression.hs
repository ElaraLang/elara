module Compiler.Transform.Expression where

import Compiler.Transform.Abstract
import Compiler.Transform.Environment
import Compiler.Transform.Types
import qualified Data.Text as T
import qualified Interpreter.AST as E
import Debug.Trace (traceShowM)

compileConstant :: E.Constant -> (JVMType, ConstantPoolEntry)
compileConstant (E.IntC i) = do
  let converted = fromInteger i :: Int -- TODO overflow but who cares
  (JVMInt, CPIntegerEntry converted)

compileExpression :: Compiler E.Expression
compileExpression (E.Bind (E.IdentifierPattern ident) (E.Constant constant)) clazz = do
  let (jvmType, compiled) = compileConstant constant
  let field =
        Field
          { accessFlags = [ACC_PUBLIC, ACC_STATIC, ACC_FINAL],
            name = T.pack $ show ident,
            descriptor = T.pack $ toInternalType jvmType,
            attributes =
              [ ConstantValueAttribute compiled
              ]
          }
  clazz {fields = field : fields clazz}
