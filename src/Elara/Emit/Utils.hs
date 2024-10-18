module Elara.Emit.Utils where
import Elara.AST.Name
import JVM.Data.Abstract.Name


createModuleName :: ModuleName -> QualifiedClassName
createModuleName (ModuleName name) = QualifiedClassName (PackageName $ init name) (ClassName $ last name)