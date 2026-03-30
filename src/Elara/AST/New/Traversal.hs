{-# LANGUAGE UndecidableInstances #-}

module Elara.AST.New.Traversal where

import Elara.AST.New.Instances ()
import Elara.AST.New.Phase
import Elara.AST.New.Types
import Prelude

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
    (Applicative f, Plated (Expr loc p)) =>
    (Expr loc p -> f (Expr loc p)) ->
    Expr loc p ->
    f (Expr loc p)
traverseExprExprs = traverseOf plate

-- | Traverse all immediate sub-patterns of a pattern node
traversePatternPats ::
    forall loc p f.
    (Applicative f, Plated (Pattern loc p)) =>
    (Pattern loc p -> f (Pattern loc p)) ->
    Pattern loc p ->
    f (Pattern loc p)
traversePatternPats = traverseOf plate

-- | Traverse all immediate sub-types of a type node
traverseTypeTypes ::
    forall loc p f.
    (Applicative f, Plated (Type loc p)) =>
    (Type loc p -> f (Type loc p)) ->
    Type loc p ->
    f (Type loc p)
traverseTypeTypes = traverseOf plate
