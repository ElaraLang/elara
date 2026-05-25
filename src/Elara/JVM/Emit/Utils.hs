module Elara.JVM.Emit.Utils where

import JVM.Data.Abstract.Name

import Elara.AST.Name

createModuleName :: ModuleName -> QualifiedClassName
createModuleName (ModuleName name) = QualifiedClassName (PackageName $ init name) (ClassName $ last name)

createQualifiedClassName :: Qualified Text -> QualifiedClassName
createQualifiedClassName (Qualified name (ModuleName mn)) =
    QualifiedClassName (PackageName (toList mn)) (ClassName name)
