{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Elara.AST.Generic.Instances.DataPlated where


import Data.Data
import Data.Generics.Wrapped
import Elara.AST.Generic.Types
import Elara.AST.Generic.Utils
import Elara.AST.Name

-- Some of these 'Plated' instances could be derived with 'template', but I feel like it's more efficient to write them by hand

instance
    RUnlocate ast =>
    Plated (Pattern' ast)
    where
    plate = traversalVL $ \f -> \case
        p@(VarPattern _) -> pure p
        ConstructorPattern a b -> ConstructorPattern a <$> traverseOf (each % _Unwrapped % _1 % traverseUnlocated @_ @ast) f b
        ListPattern a -> ListPattern <$> traverseOf (each % _Unwrapped % _1 % traverseUnlocated @_ @ast) f a
        ConsPattern a b -> ConsPattern <$> traverseOf (_Unwrapped % _1 % traverseUnlocated @_ @ast) f a <*> traverseOf (_Unwrapped . _1 . traverseUnlocated @_ @ast) f b
        WildcardPattern -> pure WildcardPattern
        IntegerPattern a -> pure (IntegerPattern a)
        FloatPattern a -> pure (FloatPattern a)
        StringPattern a -> pure (StringPattern a)
        CharPattern a -> pure (CharPattern a)
        UnitPattern -> pure UnitPattern

instance
    forall a (ast :: a).
    Data (Pattern ast) =>
    Plated (Pattern ast)
    where
    plate = template

instance
    ( RUnlocate ast
    , (DataConAs (Select "BinaryOperator" ast) (BinaryOperator ast, Expr ast, Expr ast))
    , (DataConAs (Select "InParens" ast) (Expr ast))
    ) =>
    Plated (Expr' ast)
    where
    plate f =
        let traverseExpr = (_Unwrapped . _1 . traverseUnlocated @_ @ast)
         in \case
                Int i -> pure (Int i)
                Float f -> pure (Float f)
                String s -> pure (String s)
                Char c -> pure (Char c)
                Unit -> pure Unit
                Var v -> pure (Var v)
                Constructor c -> pure (Constructor c)
                Lambda ps e -> (Lambda ps <$> traverseOf traverseExpr f e)
                FunctionCall e1 e2 -> FunctionCall <$> traverseOf traverseExpr f e1 <*> traverseOf traverseExpr f e2
                TypeApplication e1 e2 -> TypeApplication <$> traverseOf traverseExpr f e1 <*> pure e2
                If e1 e2 e3 -> If <$> traverseOf traverseExpr f e1 <*> traverseOf traverseExpr f e2 <*> traverseOf traverseExpr f e3
                List l -> List <$> traverseOf (each . traverseExpr) f l
                Match e m -> Match <$> traverseOf traverseExpr f e <*> traverseOf (each . _2 . traverseExpr) f m
                LetIn v p e1 e2 -> (LetIn v p <$> traverseOf traverseExpr f e1) <*> traverseOf traverseExpr f e2
                Let v p e -> (Let v p <$> traverseOf traverseExpr f e)
                Block b -> Block <$> traverseOf (each . traverseExpr) f b
                Tuple t -> Tuple <$> traverseOf (each . traverseExpr) f t
                BinaryOperator b ->
                    let (op, e1, e2) = dataConAs @(Select "BinaryOperator" ast) @(BinaryOperator ast, Expr ast, Expr ast) b
                     in BinaryOperator . asDataCon <$> (((,,) op <$> traverseOf traverseExpr f e1) <*> traverseOf traverseExpr f e2)
                InParens e ->
                    let e' = dataConAs @(Select "InParens" ast) @(Expr ast) e
                     in InParens . asDataCon <$> traverseOf traverseExpr f e'

instance
    forall a (ast :: a).
    Data (Expr ast) =>
    Plated (Expr ast)
    where
    plate = template

instance
    forall a (ast :: a).
    Data (Type ast) =>
    Plated (Type ast)
    where
    plate = template

instance
    forall a (ast :: a).
    ( Data (ASTLocate ast (Type' ast))
    , Data (ASTLocate ast (Select "TypeVar" ast))
    , Data (Select "TypeVar" ast)
    , Data (ASTLocate ast (Select "UserDefinedType" ast))
    , Data (ASTLocate ast LowerAlphaName)
    , Data (Select "UserDefinedType" ast)
    , Typeable ast
    , Typeable a
    , (Data (Type' ast))
    ) =>
    Plated (Type' ast)

deriving instance
    forall a (ast :: a).
    (Typeable a, Typeable ast, Data (ASTLocate ast (BinaryOperator' ast))) =>
    Data (BinaryOperator ast)

deriving instance
    forall a (ast :: a).
    (Typeable a, Typeable ast, Data (Select "Infixed" ast), Data (ASTLocate ast (Select "SymOp" ast))) =>
    Data (BinaryOperator' ast)
