module Elara.Emit.Utils where

import Elara.AST.Name
import Elara.Core (TyCon (..), Type (..))
import Elara.Core.Analysis (findTyCon)
import Elara.Prim.Core
import JVM.Data.Abstract.Name
import JVM.Data.Abstract.Type

createModuleName :: ModuleName -> QualifiedClassName
createModuleName (ModuleName name) = QualifiedClassName (PackageName $ init name) (ClassName $ last name)

createQualifiedClassName :: Qualified Text -> QualifiedClassName
createQualifiedClassName (Qualified name (ModuleName mn)) = QualifiedClassName (PackageName (toList mn)) (ClassName name)

generateFieldType :: HasCallStack => Type -> FieldType
generateFieldType (ConTy c) | c == intCon = ObjectFieldType "java.lang.Integer"
generateFieldType (ConTy c) | c == boolCon = ObjectFieldType "java.lang.Boolean"
generateFieldType (ConTy c) | c == stringCon = ObjectFieldType "java.lang.String"
generateFieldType (ConTy c) | c == charCon = ObjectFieldType "java.lang.Character"
generateFieldType (ConTy c) | c == unitCon = ObjectFieldType "Elara.Unit"
generateFieldType (ConTy (TyCon t _)) = ObjectFieldType (createQualifiedClassName t)
generateFieldType (AppTy (ConTy c) _) | c == ioCon = ObjectFieldType "Elara.IO"
generateFieldType (TyVarTy _) = ObjectFieldType "java.lang.Object"
generateFieldType (FuncTy _ _) = ObjectFieldType "Elara.Func"
generateFieldType (ForAllTy _ x) = generateFieldType x
generateFieldType x | Just (TyCon y _) <- findTyCon x = ObjectFieldType (createQualifiedClassName y)
generateFieldType o = error $ "generateFieldType: " <> show o
