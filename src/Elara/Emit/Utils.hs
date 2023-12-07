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

generateMethodDescriptor :: HasCallStack => Type -> MethodDescriptor
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
generateMethodDescriptor t@(TyVarTy{}) = MethodDescriptor [] (TypeReturn $ generateFieldType t)
generateMethodDescriptor o = error $ "generateMethodDescriptor: " <> show o

generateReturnDescriptor :: Type -> ReturnDescriptor
generateReturnDescriptor u | u == unitCon = VoidReturn
generateReturnDescriptor other = TypeReturn (generateFieldType other)

generateFieldType :: HasCallStack => Type -> FieldType
generateFieldType c | c == intCon = ObjectFieldType "java.lang.Integer"
generateFieldType c | c == boolCon = ObjectFieldType "java.lang.Boolean"
generateFieldType c | c == stringCon = ObjectFieldType "java.lang.String"
generateFieldType c | c == charCon = ObjectFieldType "java.lang.Character"
generateFieldType (AppTy l _) | l == listCon = ObjectFieldType "elara.EList"
generateFieldType (TyVarTy _) = ObjectFieldType "java.lang.Object"
generateFieldType (AppTy l _) | l == ioCon = ObjectFieldType "elara.IO"
generateFieldType (FuncTy _ _) = ObjectFieldType "elara.Func"
generateFieldType (ForAllTy _ x) = generateFieldType x
generateFieldType o = error $ "generateFieldType: " <> show o

{- | Determines if a type is a value type.
That is, a type that can be compiled to a field rather than a method.
-}
typeIsValue :: Type -> Bool
typeIsValue (ForAllTy _ x) = typeIsValue x
typeIsValue (AppTy con _) | con == ioCon = True
typeIsValue (AppTy con _) | con == listCon = True
typeIsValue c | c == stringCon = True
typeIsValue c | c == intCon = True
typeIsValue c | c == boolCon = True
typeIsValue _ = False
