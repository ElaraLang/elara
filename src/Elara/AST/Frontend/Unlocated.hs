{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Elara.AST.Frontend.Unlocated where

import Elara.AST.Frontend qualified as Frontend
import Elara.AST.Name (MaybeQualified, Name, OpName, TypeName, VarName)
import Elara.AST.Region (Located (Located))
import Elara.Data.Type (Type (..))
import Prelude hiding (Op, Type)

{- | Frontend AST without location information.
     Trees that grow was getting quite frustrating, so we're stuck with this for now.
     I apologise to future me.
-}
data Expr
    = Int Integer
    | Float Double
    | String Text
    | Char Char
    | Unit
    | Var (MaybeQualified VarName)
    | Constructor (MaybeQualified TypeName)
    | Lambda [Pattern] Expr
    | FunctionCall Expr Expr
    | If Expr Expr Expr
    | BinaryOperator BinaryOperator Expr Expr
    | List [Expr]
    | LetIn (MaybeQualified VarName) [Pattern] Expr Expr
    | Let (MaybeQualified VarName) [Pattern] Expr
    | Block (NonEmpty Expr)
    | InParens Expr
    deriving (Show, Eq)

data Pattern
    = NamedPattern Text
    | ConstructorPattern (MaybeQualified TypeName) [Pattern]
    | ListPattern [Pattern]
    | WildcardPattern
    deriving (Show, Eq)

data BinaryOperator
    = Op (MaybeQualified OpName)
    | Infixed (MaybeQualified VarName)
    deriving (Show, Eq)

data TypeAnnotation = TypeAnnotation (MaybeQualified Name) (Type MaybeQualified)
    deriving (Show, Eq)

class StripLocation a b | a -> b where
    stripLocation :: a -> b

instance StripLocation (Located a) a where
    stripLocation (Located _ a) = a

instance (Functor f, StripLocation a b) => StripLocation (f a) (f b) where
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
