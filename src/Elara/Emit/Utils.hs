module Elara.Emit.Utils where

import Data.List.NonEmpty ((<|))
import Elara.AST.Name
import Elara.Core
import Elara.Prim.Core
import JVM.Data.Abstract.Descriptor
import JVM.Data.Abstract.Name
import JVM.Data.Abstract.Type

createModuleName :: ModuleName -> QualifiedClassName
createModuleName (ModuleName name) = QualifiedClassName (PackageName $ init name) (ClassName $ last name)

generateMethodDescriptor :: (HasCallStack) => Type -> MethodDescriptor
generateMethodDescriptor (ForAllTy _ t) = generateMethodDescriptor t
generateMethodDescriptor f@(FuncTy _ _) = do
  -- (a -> b) -> [a] -> [b] gets compiled to List<B> f(Func<A, B> f, List<A> l)
  let splitUpFunction :: Type -> NonEmpty Type
      splitUpFunction (FuncTy i o) = i <| splitUpFunction o
      splitUpFunction other = pure other

      allParts = splitUpFunction f

      inputs = init allParts
      output = last allParts

  MethodDescriptor (generateFieldType <$> inputs) (generateReturnDescriptor output)
generateMethodDescriptor o = error $ "generateMethodDescriptor: " <> show o

generateReturnDescriptor :: Type -> ReturnDescriptor
generateReturnDescriptor u | u == unitCon = VoidReturn
generateReturnDescriptor other = TypeReturn (generateFieldType other)

generateFieldType :: Type -> FieldType
generateFieldType c | c == intCon = ObjectFieldType "java.lang.Integer"
generateFieldType c | c == stringCon = ObjectFieldType "java.lang.String"
generateFieldType (AppTy l _) | l == listCon = ObjectFieldType "elara.EList"
generateFieldType (TyVarTy _) = ObjectFieldType "java.lang.Object"
generateFieldType (AppTy l _) | l == ioCon = ObjectFieldType "elara.IO"
generateFieldType (FuncTy _ _) = ObjectFieldType "elara.Func"
generateFieldType o = error $ "generateFieldType: " <> show o
