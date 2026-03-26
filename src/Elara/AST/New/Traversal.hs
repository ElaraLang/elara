{-# LANGUAGE UndecidableInstances #-}

module Elara.AST.New.Traversal where

import Elara.AST.New.Phase
import Elara.AST.New.Types
import Prelude hiding (group)

{- | Traversal class for phase-specific extension constructors.
Each phase that has non-Void extensions must provide an instance.
Phases with Void extensions get trivial instances (just use absurd).
-}
class TraverseExtensions p where
    -- | Traverse all sub-expressions within an expression extension
    traverseExpressionExtensionExprs ::
        forall loc f.
        Applicative f =>
        (Expr loc p -> f (Expr loc p)) ->
        ExpressionExtension p loc ->
        f (ExpressionExtension p loc)

    -- | Traverse all sub-patterns within a pattern extension
    traversePatternExtensionPats ::
        forall loc f.
        Applicative f =>
        (Pattern loc p -> f (Pattern loc p)) ->
        PatternExtension p loc ->
        f (PatternExtension p loc)

    -- | Traverse all sub-types within a type extension
    traverseTypeExtensionTypes ::
        forall loc f.
        Applicative f =>
        (Type loc p -> f (Type loc p)) ->
        TypeSyntaxExtension p loc ->
        f (TypeSyntaxExtension p loc)

-- | Traverse all immediate sub-expressions of an expression node
traverseExprExprs ::
    forall loc p f.
    (Applicative f, TraverseExtensions p) =>
    (Expr loc p -> f (Expr loc p)) ->
    Expr' loc p ->
    f (Expr' loc p)
traverseExprExprs f = \case
    EInt i -> pure (EInt i)
    EFloat d -> pure (EFloat d)
    EString s -> pure (EString s)
    EChar c -> pure (EChar c)
    EUnit -> pure EUnit
    EVar ext v -> pure (EVar ext v)
    ECon ext c -> pure (ECon ext c)
    ELam ext binder body -> ELam ext binder <$> f body
    EApp ext e1 e2 -> EApp ext <$> f e1 <*> f e2
    ETyApp e t -> ETyApp <$> f e <*> pure t
    EIf c t e -> EIf <$> f c <*> f t <*> f e
    EMatch scrut alts -> EMatch <$> f scrut <*> traverse (\(p, e) -> (p,) <$> f e) alts
    ELetIn ext b v body -> ELetIn ext b <$> f v <*> f body
    ELet ext b v -> ELet ext b <$> f v
    EBlock es -> EBlock <$> traverse f es
    EAnn e t -> EAnn <$> f e <*> pure t
    EExtension ext -> EExtension <$> traverseExpressionExtensionExprs @p f ext

-- | Traverse all immediate sub-patterns of a pattern node
traversePatternPats ::
    forall loc p f.
    (Applicative f, TraverseExtensions p) =>
    (Pattern loc p -> f (Pattern loc p)) ->
    Pattern' loc p ->
    f (Pattern' loc p)
traversePatternPats f = \case
    PVar v -> pure (PVar v)
    PCon c ps -> PCon c <$> traverse f ps
    PWildcard -> pure PWildcard
    PInt i -> pure (PInt i)
    PFloat d -> pure (PFloat d)
    PString s -> pure (PString s)
    PChar c -> pure (PChar c)
    PUnit -> pure PUnit
    PExtension ext -> PExtension <$> traversePatternExtensionPats @p f ext

-- | Traverse all immediate sub-types of a type node
traverseTypeTypes ::
    forall loc p f.
    (Applicative f, TraverseExtensions p) =>
    (Type loc p -> f (Type loc p)) ->
    Type' loc p ->
    f (Type' loc p)
traverseTypeTypes f = \case
    TVar v -> pure (TVar v)
    TFun a b -> TFun <$> f a <*> f b
    TUnit -> pure TUnit
    TApp a b -> TApp <$> f a <*> f b
    TUserDefined name -> pure (TUserDefined name)
    TRecord fields -> TRecord <$> traverse (\(n, t) -> (n,) <$> f t) fields
    TList t -> TList <$> f t
    TExtension ext -> TExtension <$> traverseTypeExtensionTypes @p f ext
