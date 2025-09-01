{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Elara.AST.Generic.Instances.DataPlated where

import Data.Data
import Data.Generics.Wrapped
import Elara.AST.Generic.Types
import Elara.AST.Generic.Utils

-- Some of these 'Plated' instances could be derived with 'template', but I feel like it's more efficient to write them by hand

instance
    ( RUnlocate ast
    , DataConAs (Select "ListPattern" ast) [Pattern ast]
    , DataConAs (Select "ConsPattern" ast) (Pattern ast, Pattern ast)
    ) =>
    Plated (Pattern' ast)
    where
    plate = traversalVL $ \f ->
        let traversePattern = (_Unwrapped % _1 % traverseUnlocated @_ @ast)
         in \case
                p@(VarPattern _) -> pure p
                ConstructorPattern a b -> ConstructorPattern a <$> traverseOf (each % traversePattern) f b
                ListPattern a ->
                    let a' = dataConAs @(Select "ListPattern" ast) @[Pattern ast] a
                     in ListPattern . asDataCon <$> traverseOf (each % traversePattern) f a'
                ConsPattern a -> do
                    let (a1, a2) = dataConAs @(Select "ConsPattern" ast) @(Pattern ast, Pattern ast) a
                    a1' <- traverseOf traversePattern f a1
                    a2' <- traverseOf traversePattern f a2
                    pure $ ConsPattern . asDataCon $ (a1', a2')
                WildcardPattern -> pure WildcardPattern
                IntegerPattern a -> pure (IntegerPattern a)
                FloatPattern a -> pure (FloatPattern a)
                StringPattern a -> pure (StringPattern a)
                CharPattern a -> pure (CharPattern a)
                UnitPattern -> pure UnitPattern

instance
    forall a (ast :: a).
    GPlate (Pattern ast) (Pattern ast) =>
    Plated (Pattern ast)

instance
    ( RUnlocate ast
    , DataConAs (Select "BinaryOperator" ast) (BinaryOperator ast, Expr ast, Expr ast)
    , DataConAs (Select "InParens" ast) (Expr ast)
    , DataConAs (Select "List" ast) [Expr ast]
    , DataConAs (Select "Tuple" ast) (NonEmpty (Expr ast))
    ) =>
    Plated (Expr' ast)
    where
    plate = traversalVL $ \f ->
        let traverseExpr = (_Unwrapped % _1 % traverseUnlocated @_ @ast)
         in \case
                Int i -> pure (Int i)
                Float f -> pure (Float f)
                String s -> pure (String s)
                Char c -> pure (Char c)
                Unit -> pure Unit
                Var v -> pure (Var v)
                Constructor c -> pure (Constructor c)
                Lambda ps e -> Lambda ps <$> traverseOf traverseExpr f e
                FunctionCall e1 e2 -> FunctionCall <$> traverseOf traverseExpr f e1 <*> traverseOf traverseExpr f e2
                TypeApplication e1 e2 -> TypeApplication <$> traverseOf traverseExpr f e1 <*> pure e2
                If e1 e2 e3 -> If <$> traverseOf traverseExpr f e1 <*> traverseOf traverseExpr f e2 <*> traverseOf traverseExpr f e3
                List l ->
                    let l' = dataConAs @(Select "List" ast) @[Expr ast] l
                     in List . asDataCon <$> traverseOf (each % traverseExpr) f l'
                Match e m -> Match <$> traverseOf traverseExpr f e <*> traverseOf (each % _2 % traverseExpr) f m
                LetIn v p e1 e2 -> (LetIn v p <$> traverseOf traverseExpr f e1) <*> traverseOf traverseExpr f e2
                Let v p e -> Let v p <$> traverseOf traverseExpr f e
                Block b -> Block <$> traverseOf (each % traverseExpr) f b
                Tuple t ->
                    let t' = dataConAs @(Select "Tuple" ast) @(NonEmpty (Expr ast)) t
                     in Tuple . asDataCon <$> traverseOf (each % traverseExpr) f t'
                BinaryOperator b -> do
                    let (op, e1, e2) = dataConAs @(Select "BinaryOperator" ast) @(BinaryOperator ast, Expr ast, Expr ast) b
                    e1' <- traverseOf traverseExpr f e1
                    e2' <- traverseOf traverseExpr f e2
                    pure $ BinaryOperator . asDataCon $ (op, e1', e2')
                InParens e ->
                    let e' = dataConAs @(Select "InParens" ast) @(Expr ast) e
                     in InParens . asDataCon <$> traverseOf traverseExpr f e'

instance
    forall a (ast :: a).
    GPlate (Expr ast) (Expr ast) =>
    Plated (Expr ast)

instance
    forall a (ast :: a).
    GPlate (Type ast) (Type ast) =>
    Plated (Type ast)

instance
    forall a (ast :: a).
    GPlate (Type' ast) (Type' ast) =>
    Plated (Type' ast)

deriving instance
    forall a (ast :: a).
    (Typeable a, Typeable ast, Data (ASTLocate ast (BinaryOperator' ast))) =>
    Data (BinaryOperator ast)

deriving instance
    forall a (ast :: a).
    (Typeable a, Typeable ast, Data (Select "Infixed" ast), Data (ASTLocate ast (Select "SymOp" ast))) =>
    Data (BinaryOperator' ast)
