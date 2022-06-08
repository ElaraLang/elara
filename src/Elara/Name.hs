module Elara.Name where

import Data.Text (Text, unpack)
import Elara.String as ES

data Name
  = VarName ES.String
  | OpName ES.String
  | TypeName ES.String -- Type (or module) name
  | QualifiedName Name Name

instance Show Name where
  show (VarName s) = unpack s
  show (OpName s) = "(" ++ (unpack s) ++ ")"
  show (TypeName s) = unpack s
  show (QualifiedName n1 n2) = show n1 ++ "." ++ show n2

_main :: Name
_main = VarName "main"

_Main :: Name
_Main = TypeName "Main"

value :: Name -> Text
value (VarName s) = s

stringValue :: Name -> Prelude.String
stringValue = unpack . value