module Elara.Emit.Utils where

import Data.List.NonEmpty ((<|))
import Elara.AST.Name
import Elara.Core
import Elara.Core.Analysis (findTyCon)
import Elara.Prim.Core
import JVM.Data.Abstract.Descriptor
import JVM.Data.Abstract.Name
import JVM.Data.Abstract.Type

createModuleName :: ModuleName -> QualifiedClassName
createModuleName (ModuleName name) = QualifiedClassName (PackageName $ init name) (ClassName $ last name)

createQualifiedClassName :: Qualified Text -> QualifiedClassName
createQualifiedClassName (Qualified name (ModuleName mn)) = QualifiedClassName (PackageName (toList mn)) (ClassName name)

{- | Creates a qualified class name for an inner class
>>> createQualifiedInnerClassName "Inner" (QualifiedClassName (PackageName "com.example") (ClassName "Outer"))
QualifiedClassName (PackageName "com.example") (ClassName "Outer$Inner")
-}
createQualifiedInnerClassName :: Text -> QualifiedClassName -> QualifiedClassName
createQualifiedInnerClassName name (QualifiedClassName (PackageName p) (ClassName c)) = QualifiedClassName (PackageName p) (ClassName $ c <> "$" <> name)

generateMethodDescriptor :: HasCallStack => Type -> MethodDescriptor
generateMethodDescriptor x = case generateMethodDescriptor' x of
    Just y -> y
    Nothing -> error $ "generateMethodDescriptor: " <> show x

{- |
Attempts to generate a method descriptor for a given type, returning `Nothing` if the type should not compile to a method.

This follows a few simple rules:
1. @Forall@s are totally ignored due to JVM type erasure
2. Generally we merge curried functions back into a single method, for example:
@(a -> b) -> [a] -> [b]@ gets compiled to @List<B> f(Func<A, B> f, List<A> l)@
-}
generateMethodDescriptor' :: HasCallStack => Type -> Maybe MethodDescriptor
generateMethodDescriptor' (ForAllTy _ t) = generateMethodDescriptor' t
generateMethodDescriptor' f@(FuncTy _ _) = do
    let splitUpFunction :: Type -> NonEmpty Type
        splitUpFunction (FuncTy i o) = i <| splitUpFunction o
        splitUpFunction other = pure other

        allParts = splitUpFunction f

        inputs = init allParts
        output = last allParts

    pure $ MethodDescriptor (generateFieldType <$> inputs) (generateReturnDescriptor output)
generateMethodDescriptor' t@(TyVarTy{}) = Just $ MethodDescriptor [] (TypeReturn $ generateFieldType t)
-- Awkwardly we have to assume here that the constructor has no parameters with no safety checking as literally all the information we have is the name.
-- Should probably refactor ConTy to take a DataCon instead
generateMethodDescriptor' (ConTy (TyCon dc _)) = Just $ MethodDescriptor [] (TypeReturn $ ObjectFieldType $ createQualifiedClassName dc)
generateMethodDescriptor' (AppTy t _) = generateMethodDescriptor' t -- type erasure
generateMethodDescriptor' _ = Nothing

-- | Returns either the JVM type of the argument, or the JVM type of the return type, if it would compile to a method
generateReturnType :: HasCallStack => Type -> ReturnDescriptor
generateReturnType y = case generateMethodDescriptor' y of
    Just (MethodDescriptor _ x) -> x
    Nothing -> generateReturnDescriptor y

generateReturnDescriptor :: HasCallStack => Type -> ReturnDescriptor
generateReturnDescriptor (ConTy u) | u == unitCon = VoidReturn
generateReturnDescriptor other = TypeReturn (generateFieldType other)

generateFieldType :: HasCallStack => Type -> FieldType
generateFieldType (ConTy c) | c == intCon = ObjectFieldType "java.lang.Integer"
generateFieldType (ConTy c) | c == boolCon = ObjectFieldType "java.lang.Boolean"
generateFieldType (ConTy c) | c == stringCon = ObjectFieldType "java.lang.String"
generateFieldType (ConTy c) | c == charCon = ObjectFieldType "java.lang.Character"
generateFieldType (ConTy c) | c == unitCon = ObjectFieldType "Elara.Unit"
generateFieldType (ConTy (TyCon t _)) = ObjectFieldType (createQualifiedClassName t)
generateFieldType (AppTy (ConTy c) _) | c == listCon = ObjectFieldType "Elara.EList"
generateFieldType (AppTy (ConTy c) _) | c == ioCon = ObjectFieldType "Elara.IO"
generateFieldType (TyVarTy _) = ObjectFieldType "java.lang.Object"
generateFieldType (FuncTy _ _) = ObjectFieldType "Elara.Func"
generateFieldType (ForAllTy _ x) = generateFieldType x
generateFieldType x | Just (TyCon y _) <- findTyCon x = ObjectFieldType (createQualifiedClassName y)
generateFieldType o = error $ "generateFieldType: " <> show o

{- | Determines if a type is a value type.
That is, a type that can be compiled to a field rather than a method.
-}
typeIsValue :: Type -> Bool
typeIsValue (ForAllTy _ x) = typeIsValue x
typeIsValue (AppTy (ConTy con) _) | con == ioCon = True
typeIsValue (AppTy (ConTy con) _) | con == listCon = True
typeIsValue x | Just (TyCon y _) <- findTyCon x = True
typeIsValue _ = False
