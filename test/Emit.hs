module Emit where

import Elara.AST.Name
import Elara.AST.VarRef
import Elara.Core
import Elara.Data.Kind
import Elara.Data.Unique
import Elara.Emit.Var (transformTopLevelLambdas, JVMBinder(..), replaceVar)
import Test.Hspec
import Print (printPretty)

spec :: Spec
spec = do
    let var = ( Id
                    (Local (Identity (Unique "a" 7)))
                    (AppTy (ConTy (Qualified{_qualifiedName = "[]", _qualifiedQualifier = ModuleName ("Elara" :| ["Prim"])})) (TyVarTy (TypeVariable{tvName = Unique (Just "a") 6, tvKind = TypeKind})))
                )
    let x =
            Lam
               var 
                ( Lam
                    (Id (Local (Identity (Unique "b" 8))) (AppTy (ConTy (Qualified{_qualifiedName = "[]", _qualifiedQualifier = ModuleName ("Elara" :| ["Prim"])})) (TyVarTy (TypeVariable{tvName = Unique (Just "a") 6, tvKind = TypeKind}))))
                    ( Match
                        (Var (Id (Local (Identity (Unique "a" 7))) (ForAllTy (TypeVariable{tvName = Unique (Just "a") 6, tvKind = TypeKind}) (AppTy (ConTy (Qualified{_qualifiedName = "[]", _qualifiedQualifier = ModuleName ("Elara" :| ["Prim"])})) (TyVarTy (TypeVariable{tvName = Unique (Just "a") 6, tvKind = TypeKind}))))))
                        (Just (Id (Local (Identity (Unique "a" 57))) (ForAllTy (TypeVariable{tvName = Unique (Just "a") 6, tvKind = TypeKind}) (AppTy (ConTy (Qualified{_qualifiedName = "[]", _qualifiedQualifier = ModuleName ("Elara" :| ["Prim"])})) (TyVarTy (TypeVariable{tvName = Unique (Just "a") 6, tvKind = TypeKind}))))))
                        [
                        ]
                    )
                )

    let z = transformTopLevelLambdas x
    let z' = replaceVar (Normal var) (JVMLocal 0) (fmap Normal x)
    describe "Should work" $ do
        runIO $ print z'
    pure ()