module Compiler.Transform.Types where

import qualified Interpreter.AST as E

data JVMType = JVMVoid | JVMBoolean | JVMByte | JVMShort | JVMInt | JVMLong | JVMFloat | JVMDouble | JVMChar | JVMObject String | JVMArray JVMType
  deriving (Eq, Show)

toInternalType :: JVMType -> String
toInternalType JVMVoid = "V"
toInternalType JVMBoolean = "Z"
toInternalType JVMByte = "B"
toInternalType JVMShort = "S"
toInternalType JVMInt = "I"
toInternalType JVMLong = "J"
toInternalType JVMFloat = "F"
toInternalType JVMDouble = "D"
toInternalType JVMChar = "C"
toInternalType (JVMObject s) = "L" ++ s ++ ";"
toInternalType (JVMArray t) = "[" ++ toInternalType t

elaraTypeToJVMType :: E.Type -> JVMType
elaraTypeToJVMType (E.NamedType "Int") = JVMInt
elaraTypeToJVMType (E.NamedType "Boolean") = JVMBoolean
elaraTypeToJVMType (E.NamedType "Char") = JVMChar
elaraTypeToJVMType (E.NamedType "Byte") = JVMByte
elaraTypeToJVMType (E.NamedType "Short") = JVMShort
elaraTypeToJVMType (E.NamedType "Long") = JVMLong
elaraTypeToJVMType (E.NamedType "Float") = JVMFloat
elaraTypeToJVMType (E.NamedType "Double") = JVMDouble
elaraTypeToJVMType (E.NamedType "String") = JVMObject "java/lang/String"
elaraTypeToJVMType (E.NamedType s) = JVMObject s
