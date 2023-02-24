{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Elara.AST.Frontend.StripLocation where

import Elara.AST.Frontend qualified as Frontend
import Elara.AST.Frontend.Unlocated as Unlocated
import Elara.AST.Module
import Elara.AST.Region
import Elara.AST.Select
import Prelude hiding (Op, Type)

class StripLocation a b | a -> b where
    stripLocation :: a -> b

instance StripLocation (Located a) a where
    stripLocation (Located _ a) = a

instance {-# OVERLAPPABLE #-} (Functor f, StripLocation a b) => StripLocation (f a) (f b) where
    stripLocation = fmap stripLocation

instance StripLocation Frontend.Expr Expr where
    stripLocation (Frontend.Expr (Located _ expr)) = case expr of
        Frontend.Int i -> Int i
        Frontend.Float f -> Float f
        Frontend.String s -> String s
        Frontend.Char c -> Char c
        Frontend.Unit -> Unit
        Frontend.Var v -> Var (stripLocation v)
        Frontend.Constructor c -> Constructor (stripLocation c)
        Frontend.Lambda p e -> Lambda (stripLocation p) (stripLocation e)
        Frontend.FunctionCall e1 e2 -> FunctionCall (stripLocation e1) (stripLocation e2)
        Frontend.If e1 e2 e3 -> If (stripLocation e1) (stripLocation e2) (stripLocation e3)
        Frontend.BinaryOperator o e1 e2 -> BinaryOperator (stripLocation o) (stripLocation e1) (stripLocation e2)
        Frontend.List l -> List (stripLocation l)
        Frontend.LetIn v p e1 e2 -> LetIn (stripLocation v) (stripLocation p) (stripLocation e1) (stripLocation e2)
        Frontend.Let v p e -> Let (stripLocation v) (stripLocation p) (stripLocation e)
        Frontend.Block b -> Block (stripLocation b)
        Frontend.InParens e -> InParens (stripLocation e)

instance StripLocation Frontend.Pattern Pattern where
    stripLocation (Frontend.Pattern (Located _ pat)) = case pat of
        Frontend.NamedPattern n -> NamedPattern n
        Frontend.ConstructorPattern c p -> ConstructorPattern (stripLocation c) (stripLocation p)
        Frontend.ListPattern p -> ListPattern (stripLocation p)
        Frontend.WildcardPattern -> WildcardPattern

instance StripLocation Frontend.BinaryOperator BinaryOperator where
    stripLocation (Frontend.MkBinaryOperator (Located _ op)) = case op of
        Frontend.Op o -> Op (stripLocation o)
        Frontend.Infixed i -> Infixed (stripLocation i)

instance StripLocation Frontend.Type Type where
    stripLocation (Frontend.TypeVar t) = TypeVar t
    stripLocation (Frontend.FunctionType t1 t2) = FunctionType (stripLocation t1) (stripLocation t2)
    stripLocation Frontend.UnitType = UnitType
    stripLocation (Frontend.TypeConstructorApplication t1 t2) = TypeConstructorApplication (stripLocation t1) (stripLocation t2)
    stripLocation (Frontend.UserDefinedType t) = UserDefinedType (stripLocation t)

instance StripLocation (Module Frontend) (Module UnlocatedFrontend) where
    stripLocation (Module m) = Module (stripLocation (stripLocation m :: Module' Frontend))

instance StripLocation (Module' Frontend) (Module' UnlocatedFrontend) where
    stripLocation (Module' n e i d) = Module' (stripLocation n) (stripLocation e) (stripLocation i) (stripLocation d)

instance StripLocation (Exposing Frontend) (Exposing UnlocatedFrontend) where
    stripLocation ExposingAll = ExposingAll
    stripLocation (ExposingSome e) = ExposingSome (stripLocation e)

instance StripLocation (Exposition Frontend) (Exposition UnlocatedFrontend) where
    stripLocation (ExposedValue n) = ExposedValue (stripLocation n)
    stripLocation (ExposedType tn) = ExposedType (stripLocation tn)
    stripLocation (ExposedTypeAndAllConstructors tn) = ExposedTypeAndAllConstructors (stripLocation tn)
    stripLocation (ExposedOp o) = ExposedOp (stripLocation o)

instance StripLocation (Import Frontend) (Import UnlocatedFrontend) where
    stripLocation (Import m) = Import (stripLocation (stripLocation m :: Import' Frontend))

instance StripLocation (Import' Frontend) (Import' UnlocatedFrontend) where
    stripLocation (Import' i a q e) = Import' (stripLocation i) (stripLocation a) q (stripLocation e)

instance StripLocation (Declaration Frontend) (Declaration UnlocatedFrontend) where
    stripLocation (Declaration d) = Declaration (stripLocation (stripLocation d :: Declaration' Frontend))

instance StripLocation (Declaration' Frontend) (Declaration' UnlocatedFrontend) where
    stripLocation (Declaration' m n b) = Declaration' (stripLocation m) (stripLocation n) (stripLocation b)

instance StripLocation (DeclarationBody Frontend) (DeclarationBody UnlocatedFrontend) where
    stripLocation (DeclarationBody d) = DeclarationBody (stripLocation (stripLocation d :: DeclarationBody' Frontend))

instance StripLocation (DeclarationBody' Frontend) (DeclarationBody' UnlocatedFrontend) where
    stripLocation (Value e p ann) = Value (stripLocation e) (stripLocation p) (stripLocation ann)
    stripLocation (ValueTypeDef t) = ValueTypeDef (stripLocation (stripLocation t :: Maybe Frontend.TypeAnnotation))
    stripLocation (TypeAlias t) = TypeAlias (stripLocation t)

instance StripLocation Frontend.TypeAnnotation TypeAnnotation where
    stripLocation (Frontend.TypeAnnotation n t) = TypeAnnotation (stripLocation n) (stripLocation t)