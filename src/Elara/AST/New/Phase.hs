{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Elara.AST.New.Phase where

import Data.Kind qualified as Kind
import Elara.AST.Region (Located, SourceRegion)

-- | Marker for extension fields that carry no additional data
data NoExtension = NoExtension
    deriving (Show, Eq, Ord, Generic)

{- | Conditionally wrap a value in 'Located' based on the location type.
When @loc ~ SourceRegion@, wraps in 'Located' (which carries 'SourceRegion').
When @loc ~ ()@, the value is bare (no location wrapper).
-}
type family Locate (loc :: Kind.Type) (a :: Kind.Type) :: Kind.Type where
    Locate () a = a
    Locate SourceRegion a = Located a

-- | The main phase class. Each compilation stage provides an instance.
class ElaraPhase p where
    -- Occurrence types (what a use-site reference looks like)
    type ValueOccurrence p (loc :: Kind.Type) :: Kind.Type
    type ConstructorOccurrence p (loc :: Kind.Type) :: Kind.Type
    type TypeOccurrence p (loc :: Kind.Type) :: Kind.Type
    type OperatorOccurrence p (loc :: Kind.Type) :: Kind.Type
    type InfixedOccurrence p (loc :: Kind.Type) :: Kind.Type

    -- Binder types (what a binding site looks like)
    type ValueBinder p (loc :: Kind.Type) :: Kind.Type
    type TopValueBinder p (loc :: Kind.Type) :: Kind.Type
    type TopTypeBinder p (loc :: Kind.Type) :: Kind.Type
    type TypeVariable p (loc :: Kind.Type) :: Kind.Type
    type ConstructorBinder p (loc :: Kind.Type) :: Kind.Type

    -- Lambda binder (loc-dependent because it may contain patterns)
    type LambdaBinder p (loc :: Kind.Type) :: Kind.Type

    -- Per-node metadata (analysis facts, NOT syntax)
    type ExpressionMeta p (loc :: Kind.Type) :: Kind.Type
    type PatternMeta p (loc :: Kind.Type) :: Kind.Type
    type TypeMeta p (loc :: Kind.Type) :: Kind.Type

    -- Per-constructor extension fields
    type VariableExtension p :: Kind.Type
    type LambdaExtension p :: Kind.Type
    type LetExtension p :: Kind.Type
    type ApplicationExtension p :: Kind.Type
    type ConstructorNodeExtension p :: Kind.Type

    -- Extension constructors (for phase-specific syntax)
    type ExpressionExtension p (loc :: Kind.Type) :: Kind.Type
    type PatternExtension p (loc :: Kind.Type) :: Kind.Type
    type TypeSyntaxExtension p (loc :: Kind.Type) :: Kind.Type
    type DeclBodyExtension p (loc :: Kind.Type) :: Kind.Type

    -- Declaration-level metadata
    type ValueDeclMetadata p (loc :: Kind.Type) :: Kind.Type
    type TypeDeclMetadata p (loc :: Kind.Type) :: Kind.Type
