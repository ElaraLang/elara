{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{- | A `Context` is an ordered list of `Entry`s used as the state for the
    bidirectional type-checking algorithm
-}
module Elara.TypeInfer.Context (
  -- * Types
  Entry (..),
  Context,

  -- * Utilities
  lookup,
  tryFinaliseType,
  splitOnUnsolvedType,
  splitOnUnsolvedFields,
  splitOnUnsolvedAlternatives,
  discardUpTo,
  solveType,
  solveRecord,
  solveUnion,
  complete,
) where

import Elara.AST.Name
import Elara.TypeInfer.Domain (Domain)
import Elara.TypeInfer.Existential (Existential)
import Elara.TypeInfer.Monotype (Monotype)
import Elara.TypeInfer.Type (Type)
import Prelude hiding (group)

import Control.Monad qualified as Monad
import Control.Monad.State.Strict qualified as State

import Elara.AST.VarRef (IgnoreLocVarRef)
import Elara.Data.Pretty
import Elara.TypeInfer.Domain qualified as Domain
import Elara.TypeInfer.Existential qualified as Existential
import Elara.TypeInfer.Monotype qualified as Monotype
import Elara.TypeInfer.Type qualified as Type

{- $setup

   >>> :set -XOverloadedStrings
   >>> :set -XTypeApplications
   >>> import Grace.Type (Record, Union)
-}

-- | An element of the `Context` list
data Entry s
  = -- | Universally quantified variable
    --
    -- >>> pretty @(Entry ()) (Variable Domain.Type "a")
    -- a: Type
    Variable Domain Text
  | -- | A bound variable whose type is known
    --
    -- >>> pretty @(Entry ()) (Annotation "x" "a")
    -- x: a
    Annotation (IgnoreLocVarRef Name) (Type s)
  | -- | A placeholder type variable whose type has not yet been inferred
    --
    -- >>> pretty @(Entry ()) (UnsolvedType 0)
    -- a?
    UnsolvedType (Existential Monotype)
  | -- | A placeholder fields variable whose type has not yet been inferred
    --
    -- >>> pretty @(Entry ()) (UnsolvedFields 0)
    -- a?
    UnsolvedFields (Existential Monotype.Record)
  | -- | A placeholder alternatives variable whose type has not yet been
    -- inferred
    --
    -- >>> pretty @(Entry ()) (UnsolvedAlternatives 0)
    -- a?
    UnsolvedAlternatives (Existential Monotype.Union)
  | -- | A placeholder type variable whose type has been (at least partially)
    --   inferred
    --
    -- >>> pretty @(Entry ()) (SolvedType 0 (Monotype.Scalar Monotype.Bool))
    -- a = Bool
    SolvedType (Existential Monotype) Monotype
  | -- | A placeholder fields variable whose type has been (at least partially)
    --   inferred
    --
    -- >>> pretty @(Entry ()) (SolvedFields 0 (Monotype.Fields [("x", "X")] (Monotype.UnsolvedFields 1)))
    -- a = x: X, b?
    SolvedFields (Existential Monotype.Record) Monotype.Record
  | -- | A placeholder alternatives variable whose type has been (at least
    --   partially) inferred
    --
    -- >>> pretty @(Entry ()) (SolvedAlternatives 0 (Monotype.Alternatives [("x", "X")] (Monotype.UnsolvedAlternatives 1)))
    -- a = x: X | b?
    SolvedAlternatives (Existential Monotype.Union) Monotype.Union
  | -- | This is used by the bidirectional type-checking algorithm to separate
    --   context entries introduced before and after type-checking a universally
    --   quantified type
    --
    -- >>> pretty @(Entry ()) (MarkerType 0)
    -- ➤ a: Type
    MarkerType (Existential Monotype)
  | -- | This is used by the bidirectional type-checking algorithm to separate
    --   context entries introduced before and after type-checking universally
    --   quantified fields
    --
    -- >>> pretty @(Entry ()) (MarkerFields 0)
    -- ➤ a: Fields
    MarkerFields (Existential Monotype.Record)
  | -- | This is used by the bidirectional type-checking algorithm to separate
    --   context entries introduced before and after type-checking universally
    --   quantified alternatives
    --
    -- >>> pretty @(Entry ()) (MarkerAlternatives 0)
    -- ➤ a: Alternatives
    MarkerAlternatives (Existential Monotype.Union)
  deriving stock (Eq, Show)

{- | A `Context` is an ordered list of `Entry`s

    Note that this representation stores the `Context` entries in reverse
    order, meaning that the beginning of the list represents the entries that
    were added last.  For example, this context:

    > •, a : Bool, b, c?, d = c?, ➤e : Type

    … corresponds to this Haskell representation:

    > [ MarkerType 4
    > , SolvedType 3 (Monotype.UnsolvedType 2)
    > , UnsolvedType 2
    > , Variable "b"
    > , Annotation "a" (Monotype.Scalar Monotype.Bool)
    > ]

    The ordering matters because the bidirectional type-checking algorithm
    uses ordering of `Context` entries to determine scope.  Specifically:

    * each `Entry` in the `Context` can only refer to variables preceding it
      within the `Context`

    * the bidirectional type-checking algorithm sometimes discards all entries
      in the context past a certain entry to reflect the end of their
      \"lifetime\"
-}
type Context s = [Entry s]

{- | Substitute a `Type` using the solved entries of a `Context`

    >>> original = Type.UnsolvedType () 0
    >>> pretty @(Type ()) original
    a?

    >>> pretty @(Type ()) (solveType [ UnsolvedType 1, SolvedType 0 (Monotype.Scalar Monotype.Bool) ] original)
    Bool
-}
solveType :: Context s -> Type s -> Type s
solveType context type_ = foldl' snoc type_ context
 where
  snoc t (SolvedType a τ) = Type.solveType a τ t
  snoc t (SolvedFields a r) = Type.solveFields a r t
  snoc t (SolvedAlternatives a u) = Type.solveAlternatives a u t
  snoc t _ = t

{- | Substitute a t`Type.Record` using the solved entries of a `Context`

    >>> original = Type.Fields [("x", Type.Scalar () Monotype.Bool)] (Monotype.UnsolvedFields 0)
    >>> pretty @(Record ()) original
    { x: Bool, a? }

    >>> entry = SolvedFields 0 (Monotype.Fields [] Monotype.EmptyFields)
    >>> pretty entry
    a = •

    >>> pretty @(Record ()) (solveRecord [ entry ] original)
    { x: Bool }
-}
solveRecord :: Context s -> Type.Record s -> Type.Record s
solveRecord context oldFields = newFields
 where
  location =
    error "Grace.Context.solveRecord: Internal error - Missing location field"

  -- TODO: Come up with total solution
  Type.Record{fields = newFields} =
    solveType context Type.Record{fields = oldFields, ..}

{- | Substitute a t`Type.Union` using the solved entries of a `Context`
    `Context`

    >>> original = Type.Alternatives [("A", Type.Scalar () Monotype.Bool)] (Monotype.UnsolvedAlternatives 0)
    >>> pretty @(Union ()) original
    < A: Bool | a? >

    >>> entry = SolvedAlternatives 0 (Monotype.Alternatives [] Monotype.EmptyAlternatives)
    >>> pretty entry
    a = •

    >>> pretty @(Union ()) (solveUnion [ entry ] original)
    < A: Bool >
-}
solveUnion :: Context s -> Type.Union s -> Type.Union s
solveUnion context oldAlternatives = newAlternatives
 where
  location =
    error "Grace.Context.solveUnion: Internal error - Missing location field"

  -- TODO: Come up with total solution
  Type.Union{alternatives = newAlternatives} =
    solveType context Type.Union{alternatives = oldAlternatives, ..}

{- | This function is used at the end of the bidirectional type-checking
    algorithm to complete the inferred type by:

    * Substituting the type with the solved entries in the `Context`

    * Adding universal quantifiers for all unsolved entries in the `Context`

    >>> original = Type.Function () (Type.UnsolvedType () 1) (Type.UnsolvedType () 0)
    >>> pretty @(Type ()) original
    b? -> a?

    >>> pretty @(Type ()) (complete [ UnsolvedType 1, SolvedType 0 (Monotype.Scalar Monotype.Bool) ] original)
    forall (a : Type) . a -> Bool
-}
complete :: Context s -> Type s -> Type s
complete context type0 = State.evalState (Monad.foldM snoc type0 context) 0
 where
  numUnsolved = fromIntegral (length (filter predicate context)) - 1
   where
    predicate (UnsolvedType _) = True
    predicate (UnsolvedFields _) = True
    predicate (UnsolvedAlternatives _) = True
    predicate _ = False

  snoc t (SolvedType a τ) = pure (Type.solveType a τ t)
  snoc t (SolvedFields a r) = pure (Type.solveFields a r t)
  snoc t (SolvedAlternatives a r) = pure (Type.solveAlternatives a r t)
  snoc t (UnsolvedType a) | a `Type.typeFreeIn` t = do
    n <- State.get

    State.put $! n + 1

    let name = Existential.toVariable (numUnsolved - n)

    let domain = Domain.Type

    let type_ = Type.solveType a (Monotype.VariableType name) t

    let location = Type.location t

    let nameLocation = location

    pure Type.Forall{..}
  snoc t (UnsolvedFields p) | p `Type.fieldsFreeIn` t = do
    n <- State.get

    State.put $! n + 1

    let name = Existential.toVariable (numUnsolved - n)

    let domain = Domain.Fields

    let type_ = Type.solveFields p (Monotype.Fields [] (Monotype.VariableFields name)) t

    let location = Type.location t

    let nameLocation = location

    pure Type.Forall{..}
  snoc t (UnsolvedAlternatives p) | p `Type.alternativesFreeIn` t = do
    n <- State.get

    State.put $! n + 1

    let name = Existential.toVariable (numUnsolved - n)

    let domain = Domain.Alternatives

    let type_ = Type.solveAlternatives p (Monotype.Alternatives [] (Monotype.VariableAlternatives name)) t

    let location = Type.location t

    let nameLocation = location

    pure Type.Forall{..}
  snoc t _ = pure t

-- | Checks if a type is complete, i.e. it has no unsolved variables.
isComplete :: Type s -> Bool
isComplete Type.UnsolvedType{} = False
isComplete _ = True

{- | Uses duplicate annotations to resolve any fixable-ambiguity a Type.
For example, say we have 3 annotations:
     @
     id : ∀ a. (a -> a)
     main: b?
     id: b?
     @
The duplicate annotations for @b@ imply that @b? ~ ∀ a. (a -> a)@ from which we can derive
that @main: ∀ a. (a -> a)@. That's what this function does.
If we passed a second argument of @main@ we would first look for annotations with the value @b?@ and then
use the duplicate annotations to resolve the ambiguity.
-}
tryFinaliseType :: (Eq s, Ord s) => Context s -> IgnoreLocVarRef Name -> _
tryFinaliseType ctx name = do
  let annotations = lookupAll name ctx
  let y = do
        ann <- annotations
        case ann of
          mono@(Type.UnsolvedType _ _) ->
            concatMap
              ( \case
                  Annotation n m | n /= name && m == mono -> ordNub $ filter isComplete (lookupAll n ctx)
                  _ -> []
              )
              ctx
          _ -> []

  viaNonEmpty head y

{- | Split a `Context` into two `Context`s before and after the given
    `UnsolvedType` variable.  Neither `Context` contains the variable

    Returns `Nothing` if no such `UnsolvedType` variable is present within the
    `Context`

    >>> splitOnUnsolvedType 1 [ UnsolvedType 1, SolvedType 0 (Monotype.Scalar Monotype.Bool) ]
    Just ([],[SolvedType 0 (Scalar Bool)])
    >>> splitOnUnsolvedType 0 [ UnsolvedType 1, SolvedType 0 (Monotype.Scalar Monotype.Bool) ]
    Nothing
-}
splitOnUnsolvedType ::
  -- | `UnsolvedType` variable to split on
  Existential Monotype ->
  Context s ->
  Maybe (Context s, Context s)
splitOnUnsolvedType a0 (UnsolvedType a1 : entries) | a0 == a1 = pure ([], entries)
splitOnUnsolvedType a (entry : entries) = do
  (prefix, suffix) <- splitOnUnsolvedType a entries
  pure (entry : prefix, suffix)
splitOnUnsolvedType _ [] = Nothing

{- | Split a `Context` into two `Context`s before and after the given
    `UnsolvedFields` variable.  Neither `Context` contains the variable

    Returns `Nothing` if no such `UnsolvedFields` variable is present within the
    `Context`

    >>> splitOnUnsolvedFields 1 [ UnsolvedFields 1, SolvedType 0 (Monotype.Scalar Monotype.Bool) ]
    Just ([],[SolvedType 0 (Scalar Bool)])
    >>> splitOnUnsolvedFields 0 [ UnsolvedFields 1, SolvedType 0 (Monotype.Scalar Monotype.Bool) ]
    Nothing
-}
splitOnUnsolvedFields ::
  -- | `UnsolvedFields` variable to split on
  Existential Monotype.Record ->
  Context s ->
  Maybe (Context s, Context s)
splitOnUnsolvedFields p0 (UnsolvedFields p1 : entries)
  | p0 == p1 = pure ([], entries)
splitOnUnsolvedFields p (entry : entries) = do
  (prefix, suffix) <- splitOnUnsolvedFields p entries
  pure (entry : prefix, suffix)
splitOnUnsolvedFields _ [] = Nothing

{- | Split a `Context` into two `Context`s before and after the given
    `UnsolvedAlternatives` variable.  Neither `Context` contains the variable

    Returns `Nothing` if no such `UnsolvedAlternatives` variable is present
    within the `Context`

    >>> splitOnUnsolvedAlternatives 1 [ UnsolvedAlternatives 1, SolvedType 0 (Monotype.Scalar Monotype.Bool) ]
    Just ([],[SolvedType 0 (Scalar Bool)])
    >>> splitOnUnsolvedAlternatives 0 [ UnsolvedAlternatives 1, SolvedType 0 (Monotype.Scalar Monotype.Bool) ]
    Nothing
-}
splitOnUnsolvedAlternatives ::
  -- | `UnsolvedAlternatives` variable to split on
  Existential Monotype.Union ->
  Context s ->
  Maybe (Context s, Context s)
splitOnUnsolvedAlternatives p0 (UnsolvedAlternatives p1 : entries)
  | p0 == p1 = pure ([], entries)
splitOnUnsolvedAlternatives p (entry : entries) = do
  (prefix, suffix) <- splitOnUnsolvedAlternatives p entries
  pure (entry : prefix, suffix)
splitOnUnsolvedAlternatives _ [] = Nothing

{- | Retrieve a variable's annotated type from a `Context`, given the variable's
    label and index

    >>> lookup "x" 0 [ Annotation "x" (Type.Scalar () Monotype.Bool), Annotation "y" (Type.Scalar () Monotype.Natural) ]
    Just (Scalar {location = (), scalar = Bool})
-}
lookup ::
  -- | Variable label
  IgnoreLocVarRef Name ->
  Context s ->
  Maybe (Type s)
lookup x ctx = viaNonEmpty head (lookupAll x ctx)

-- Finds all annotations for a given variable name
lookupAll ::
  -- | Variable label
  IgnoreLocVarRef Name ->
  Context s ->
  [Type s]
lookupAll _ [] = []
lookupAll x0 (Annotation x1 _A : _Γ) | x0 == x1 = _A : lookupAll x0 _Γ
lookupAll x0 (_ : _Γ) = lookupAll x0 _Γ

{- | Discard all entries from a `Context` up to and including the given `Entry`

    >>> discardUpTo (MarkerType 1) [ UnsolvedType 1, MarkerType 1, UnsolvedType 0 ]
    [UnsolvedType 0]
-}
discardUpTo :: (Eq s) => Entry s -> Context s -> Context s
discardUpTo entry0 (entry1 : _Γ)
  | entry0 == entry1 = _Γ
  | otherwise = discardUpTo entry0 _Γ
discardUpTo _ [] = []

instance (Show s) => Pretty (Entry s) where
  pretty = prettyEntry

prettyEntry :: (Show s) => Entry s -> Doc AnsiStyle
prettyEntry (Variable domain a) =
  pretty a <> ":" <> " " <> pretty domain
prettyEntry (UnsolvedType a) =
  pretty a <> "?"
prettyEntry (UnsolvedFields p) =
  pretty p <> "?"
prettyEntry (UnsolvedAlternatives p) =
  pretty p <> "?"
prettyEntry (SolvedType a τ) =
  pretty a <> " " <> "=" <> " " <> pretty τ
prettyEntry (SolvedFields p (Monotype.Fields [] Monotype.EmptyFields)) =
  pretty p <> " " <> "=" <> " " <> "•"
prettyEntry (SolvedFields p0 (Monotype.Fields [] (Monotype.UnsolvedFields p1))) =
  pretty p0
    <> " "
    <> "="
    <> " "
    <> pretty p1
    <> "?"
prettyEntry (SolvedFields p0 (Monotype.Fields [] (Monotype.VariableFields p1))) =
  pretty p0
    <> " "
    <> "="
    <> " "
    <> pretty p1
prettyEntry (SolvedFields p (Monotype.Fields ((k0, τ0) : kτs) fields)) =
  pretty p
    <> " = "
    <> pretty k0
    <> ":"
    <> " "
    <> pretty τ0
    <> foldMap prettyFieldType kτs
    <> case fields of
      Monotype.EmptyFields ->
        ""
      Monotype.UnsolvedFields p1 ->
        "," <> " " <> pretty p1 <> "?"
      Monotype.VariableFields p1 ->
        "," <> " " <> pretty p1
prettyEntry (SolvedAlternatives p (Monotype.Alternatives [] Monotype.EmptyAlternatives)) =
  pretty p <> " " <> "=" <> " " <> "•"
prettyEntry (SolvedAlternatives p0 (Monotype.Alternatives [] (Monotype.UnsolvedAlternatives p1))) =
  pretty p0 <> " " <> "=" <> " " <> pretty p1 <> "?"
prettyEntry (SolvedAlternatives p0 (Monotype.Alternatives [] (Monotype.VariableAlternatives p1))) =
  pretty p0 <> " " <> "=" <> " " <> pretty p1
prettyEntry (SolvedAlternatives p0 (Monotype.Alternatives ((k0, τ0) : kτs) fields)) =
  pretty p0
    <> " "
    <> "="
    <> " "
    <> prettyAlternativeType (k0, τ0)
    <> foldMap (\kt -> " " <> "|" <> " " <> prettyAlternativeType kt) kτs
    <> case fields of
      Monotype.EmptyAlternatives ->
        ""
      Monotype.UnsolvedAlternatives p1 ->
        " " <> "|" <> " " <> pretty p1 <> "?"
      Monotype.VariableAlternatives p1 ->
        " " <> "|" <> " " <> pretty p1
prettyEntry (Annotation x a) = group (flatAlt long short)
 where
  long =
    align
      ( pretty x
          <> ":"
          <> hardline
          <> "  "
          <> pretty a
      )

  short = pretty x <> ":" <> " " <> pretty a
prettyEntry (MarkerType a) =
  "➤ " <> pretty a <> ": Type"
prettyEntry (MarkerFields a) =
  "➤ " <> pretty a <> ": Fields"
prettyEntry (MarkerAlternatives a) =
  "➤ " <> pretty a <> ": Alternatives"

prettyFieldType :: (Text, Monotype) -> Doc AnsiStyle
prettyFieldType (k, τ) =
  "," <> " " <> pretty k <> ":" <> " " <> pretty τ

prettyAlternativeType :: (Text, Monotype) -> Doc AnsiStyle
prettyAlternativeType (k, τ) =
  pretty k <> ":" <> " " <> pretty τ