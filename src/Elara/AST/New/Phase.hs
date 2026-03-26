{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{- | This module defines the 'ElaraPhase' type class, which abstracts over the different phases of the compiler.
The idea here is that each phase / stage has a different instance which provides different types for shared constructs,
along with adding their own phase-specific extension syntax and metadata.
-}
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

    -- | Value occurrence (variable reference)
    type ValueOccurrence p (loc :: Kind.Type) :: Kind.Type

    -- | Constructor occurrence (data constructor reference)
    type ConstructorOccurrence p (loc :: Kind.Type) :: Kind.Type

    -- | Type occurrence (type constructor reference)
    type TypeOccurrence p (loc :: Kind.Type) :: Kind.Type

    -- | Operator occurrence (operator reference)
    type OperatorOccurrence p (loc :: Kind.Type) :: Kind.Type

    -- | Infixed occurrence (either variable or constructor)
    type InfixedOccurrence p (loc :: Kind.Type) :: Kind.Type

    -- Binder types (what a binding site looks like)

    -- | Value binder name (variable binding)
    type ValueBinder p (loc :: Kind.Type) :: Kind.Type

    -- | Top-level value binder name
    type TopValueBinder p (loc :: Kind.Type) :: Kind.Type

    -- | Top-level type binder name
    type TopTypeBinder p (loc :: Kind.Type) :: Kind.Type

    -- | Type variable
    type TypeVariable p (loc :: Kind.Type) :: Kind.Type

    -- | Constructor binder (data constructor binding)
    type ConstructorBinder p (loc :: Kind.Type) :: Kind.Type

    -- | Lambda binder (the @p@ in @\p -> x@)
    type LambdaBinder p (loc :: Kind.Type) :: Kind.Type

    -- | Metadata attached to an expression. This should be for inferred / analysed information, not syntactic information
    type ExpressionMeta p (loc :: Kind.Type) :: Kind.Type

    -- | Metadata attached to a pattern
    type PatternMeta p (loc :: Kind.Type) :: Kind.Type

    -- | Metadata attached to a type
    type TypeMeta p (loc :: Kind.Type) :: Kind.Type

    -- Per-constructor extension fields
    type VariableExtension p :: Kind.Type
    type VariableExtension p = NoExtension

    type LambdaExtension p :: Kind.Type
    type LambdaExtension p = NoExtension

    type LetExtension p :: Kind.Type
    type LetExtension p = NoExtension

    type ApplicationExtension p :: Kind.Type
    type ApplicationExtension p = NoExtension

    type ConstructorNodeExtension p :: Kind.Type
    type ConstructorNodeExtension p = NoExtension

    -- Extension constructors (for phase-specific syntax)
    type ExpressionExtension p (loc :: Kind.Type) :: Kind.Type
    type PatternExtension p (loc :: Kind.Type) :: Kind.Type
    type TypeSyntaxExtension p (loc :: Kind.Type) :: Kind.Type
    type DeclBodyExtension p (loc :: Kind.Type) :: Kind.Type

    {- | Extra patterns in a value declaration (e.g. multi-arg @let f x y = ...@).
    Present in Frontend, eliminated (set to @()@) from Desugared onward.
    -}
    type ValueDeclPatterns p (loc :: Kind.Type) :: Kind.Type

    {- | User-written type annotation on a value declaration (e.g. @let x : Int = ...@).
    Present in Frontend, merged into 'ValueDeclMetadata' from Desugared onward.
    -}
    type ValueDeclTypeAnnotation p (loc :: Kind.Type) :: Kind.Type

    -- Declaration-level metadata
    type ValueDeclMetadata p (loc :: Kind.Type) :: Kind.Type
    type TypeDeclMetadata p (loc :: Kind.Type) :: Kind.Type
