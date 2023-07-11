{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Elara.TypeInfer.Type where

import Control.Lens qualified as Lens

import Control.Lens (Plated (..))
import Data.Generics.Product (the)
import Data.Generics.Sum (_As)
import Elara.TypeInfer.Domain (Domain)
import Elara.TypeInfer.Existential (Existential)

import Elara.TypeInfer.Monotype (
    Monotype,
    RemainingAlternatives (..),
    RemainingFields (..),
    Scalar (..),
 )

import Elara.AST.Region (SourceRegion)
import Elara.AST.StripLocation (StripLocation (stripLocation))
import Elara.Data.Pretty
import Elara.TypeInfer.Domain qualified as Domain
import Elara.TypeInfer.Monotype qualified as Monotype

-- | A potentially polymorphic type
data Type s
    = -- | Type variable
      --
      -- >>> pretty @(Type ()) (VariableType () "a")
      -- a
      VariableType {location :: s, name :: Text}
    | -- | A placeholder variable whose type has not yet been inferred
      --
      -- >>> pretty @(Type ()) (UnsolvedType () 0)
      -- a?
      UnsolvedType {location :: s, existential :: Existential Monotype}
    | -- | Existentially quantified type
      --
      -- >>> pretty @(Type ()) (Exists () () "a" Domain.Type "a")
      -- exists (a : Type) . a
      Exists {location :: s, nameLocation :: s, name :: Text, domain :: Domain, type_ :: Type s}
    | -- | Universally quantified type
      --
      -- >>> pretty @(Type ()) (Forall () () "a" Domain.Type "a")
      -- forall (a : Type) . a
      Forall {location :: s, nameLocation :: s, name :: Text, domain :: Domain, type_ :: Type s}
    | -- | Function type
      --
      -- >>> pretty @(Type ()) (Function () "a" "b")
      -- a -> b
      Function {location :: s, input :: Type s, output :: Type s}
    | -- | Optional type
      --
      -- >>> pretty @(Type ()) (Optional () "a")
      -- Optional a
      Optional {location :: s, type_ :: Type s}
    | -- | List type
      --
      -- >>> pretty @(Type ()) (List () "a")
      -- List a
      List {location :: s, type_ :: Type s}
    | -- | Record type
      --
      -- >>> pretty @(Type ()) (Record () (Fields [("x", "X"), ("y", "Y")] Monotype.EmptyFields))
      -- { x: X, y: Y }
      -- >>> pretty @(Type ()) (Record () (Fields [("x", "X"), ("y", "Y")] (Monotype.UnsolvedFields 0)))
      -- { x: X, y: Y, a? }
      Record {location :: s, fields :: Record s}
    | -- | Union type
      --
      -- >>> pretty @(Type ()) (Union () (Alternatives [("X", "X"), ("Y", "Y")] Monotype.EmptyAlternatives))
      -- < X: X | Y: Y >
      -- >>> pretty @(Type ()) (Union () (Alternatives [("X", "X"), ("Y", "Y")] (Monotype.UnsolvedAlternatives 0)))
      -- < X: X | Y: Y | a? >
      Union {location :: s, alternatives :: Union s}
    | Scalar {location :: s, scalar :: Scalar}
    | -- | Tuple Type
      --
      -- >>> pretty @(Type ()) (Tuple () (NonEmpty.fromList ["a", "b"]))
      -- (a, b)
      Tuple {location :: s, types :: NonEmpty (Type s)}
    | -- | A custom data type
      --
      -- >>> pretty @(Type ()) (Custom () "Maybe" ["a"])
      -- Maybe a
      Custom {location :: s, name :: Text, typeArguments :: [Type s]}
    | -- | A type alias
      --
      -- >>> pretty @(Type ()) (Alias () "Tuple2" ["a", "b"] (Tuple () (NonEmpty.fromList ["a", "b"])))
      -- type Tuple2 a b = (a, b)
      Alias {location :: s, name :: Text, typeArguments :: [Type s], value :: Type s}
    deriving (Eq, Ord, Functor, Generic, Show)

instance IsString (Type ()) where
    fromString string = VariableType{name = fromString string, location = ()}

instance Plated (Type s) where
    plate onType type_ =
        case type_ of
            VariableType{..} -> pure VariableType{..}
            UnsolvedType{..} -> pure UnsolvedType{..}
            Exists{type_ = oldType, ..} -> do
                newType <- onType oldType
                pure Exists{type_ = newType, ..}
            Forall{type_ = oldType, ..} -> do
                newType <- onType oldType
                pure Forall{type_ = newType, ..}
            Function{input = oldInput, output = oldOutput, ..} -> do
                newInput <- onType oldInput
                newOutput <- onType oldOutput
                pure Function{input = newInput, output = newOutput, ..}
            Optional{type_ = oldType, ..} -> do
                newType <- onType oldType
                pure Optional{type_ = newType, ..}
            List{type_ = oldType, ..} -> do
                newType <- onType oldType
                pure List{type_ = newType, ..}
            Tuple{types = oldTypes, ..} -> do
                newTypes <- traverse onType oldTypes
                pure Tuple{types = newTypes, ..}
            Record{fields = Fields oldFieldTypes remainingFields, ..} -> do
                let onPair (field, oldType) = do
                        newType <- onType oldType
                        pure (field, newType)
                newFieldTypes <- traverse onPair oldFieldTypes
                pure Record{fields = Fields newFieldTypes remainingFields, ..}
            Union{alternatives = Alternatives oldAlternativeTypes remainingAlternatives, ..} -> do
                let onPair (alternative, oldType) = do
                        newType <- onType oldType
                        pure (alternative, newType)
                newAlternativeTypes <- traverse onPair oldAlternativeTypes
                pure Union{alternatives = Alternatives newAlternativeTypes remainingAlternatives, ..}
            Scalar{..} -> pure Scalar{..}
            Custom{typeArguments = oldTypeArguments, ..} -> do
                newTypeArguments <- traverse onType oldTypeArguments
                pure Custom{typeArguments = newTypeArguments, ..}
            Alias{typeArguments = oldTypeArguments, value = oldValue, ..} -> do
                newTypeArguments <- traverse onType oldTypeArguments
                newValue <- onType oldValue
                pure Alias{value = newValue, typeArguments = newTypeArguments, ..}

-- | A potentially polymorphic record type
data Record s = Fields [(Text, Type s)] RemainingFields
    deriving (Eq, Ord, Functor, Generic, Show)

-- | A potentially polymorphic union type
data Union s = Alternatives [(Text, Type s)] RemainingAlternatives
    deriving (Eq, Ord, Functor, Generic, Show)

{- | This function should not be exported or generally used because it does not
    handle the `location` field correctly.  It is only really safe to use within
    one of the @solve*@ functions
-}
fromMonotype :: Monotype -> Type ()
fromMonotype monotype =
    case monotype of
        Monotype.VariableType name ->
            VariableType{..}
        Monotype.UnsolvedType existential ->
            UnsolvedType{..}
        Monotype.Function input output ->
            Function{input = fromMonotype input, output = fromMonotype output, ..}
        Monotype.Optional type_ ->
            Optional{type_ = fromMonotype type_, ..}
        Monotype.List type_ ->
            List{type_ = fromMonotype type_, ..}
        Monotype.Record (Monotype.Fields kτs ρ) ->
            Record{fields = Fields (map (second fromMonotype) kτs) ρ, ..}
        Monotype.Union (Monotype.Alternatives kτs ρ) ->
            Union{alternatives = Alternatives (map (second fromMonotype) kτs) ρ, ..}
        Monotype.Scalar scalar ->
            Scalar{..}
        Monotype.Tuple types ->
            Tuple{types = fmap fromMonotype types, ..}
  where
    location = ()

{- | Substitute a `Type` by replacing all occurrences of the given unsolved
    variable with a `Monotype`
-}
solveType :: Existential Monotype -> Monotype -> Type s -> Type s
solveType unsolved monotype = Lens.transform transformType
  where
    transformType UnsolvedType{..}
        | unsolved == existential =
            fmap (const location) (fromMonotype monotype)
    transformType type_ =
        type_

{- | Substitute a `Type` by replacing all occurrences of the given unsolved
    fields variable with a t`Monotype.Record`
-}
solveFields ::
    Existential Monotype.Record -> Monotype.Record -> Type s -> Type s
solveFields unsolved (Monotype.Fields fieldMonotypes fields) =
    Lens.transform transformType
  where
    transformType Record{fields = Fields fieldTypes (UnsolvedFields existential), ..}
        | unsolved == existential =
            Record{fields = Fields fieldTypes' fields, ..}
      where
        fieldTypes' =
            fieldTypes <> map transformPair fieldMonotypes

        transformPair (field, monotype) =
            (field, fmap (const location) (fromMonotype monotype))
    transformType type_ =
        type_

{- | Substitute a `Type` by replacing all occurrences of the given unsolved
    alternatives variable with a t`Monotype.Union`
-}
solveAlternatives ::
    Existential Monotype.Union -> Monotype.Union -> Type s -> Type s
solveAlternatives unsolved (Monotype.Alternatives alternativeMonotypes alternatives) =
    Lens.transform transformType
  where
    transformType Union{alternatives = Alternatives alternativeTypes (UnsolvedAlternatives existential), ..}
        | unsolved == existential =
            Union{alternatives = Alternatives alternativeTypes' alternatives, ..}
      where
        alternativeTypes' =
            alternativeTypes <> map transformPair alternativeMonotypes

        transformPair (alternative, monotype) =
            (alternative, fmap (const location) (fromMonotype monotype))
    transformType type_ =
        type_

{- | Replace all occurrences of a variable within one `Type` with another `Type`,
    given the variable's  and index
-}
substituteType :: Text -> Int -> Type s -> Type s -> Type s
substituteType a n _A type_ =
    case type_ of
        VariableType{..}
            | a == name && n == 0 -> _A
            | otherwise -> VariableType{..}
        UnsolvedType{..} ->
            UnsolvedType{..}
        Exists{type_ = oldType, ..} -> Exists{type_ = newType, ..}
          where
            newType = substituteType a n' _A oldType

            n'
                | a == name && domain == Domain.Type = n + 1
                | otherwise = n
        Forall{type_ = oldType, ..} -> Forall{type_ = newType, ..}
          where
            newType = substituteType a n' _A oldType

            n'
                | a == name && domain == Domain.Type = n + 1
                | otherwise = n
        Function{input = oldInput, output = oldOutput, ..} ->
            Function{input = newInput, output = newOutput, ..}
          where
            newInput = substituteType a n _A oldInput

            newOutput = substituteType a n _A oldOutput
        Optional{type_ = oldType, ..} -> Optional{type_ = newType, ..}
          where
            newType = substituteType a n _A oldType
        List{type_ = oldType, ..} -> List{type_ = newType, ..}
          where
            newType = substituteType a n _A oldType
        Tuple{types = oldTypes, ..} ->
            Tuple{types = fmap (substituteType a n _A) oldTypes, ..}
        Record{fields = Fields kAs ρ, ..} ->
            Record{fields = Fields (map (second (substituteType a n _A)) kAs) ρ, ..}
        Union{alternatives = Alternatives kAs ρ, ..} ->
            Union{alternatives = Alternatives (map (second (substituteType a n _A)) kAs) ρ, ..}
        Scalar{..} ->
            Scalar{..}
        Custom{typeArguments = oldTypeArguments, ..} ->
            Custom{typeArguments = fmap (substituteType a n _A) oldTypeArguments, ..}
        Alias{value = oldValue, ..} ->
            Alias{value = substituteType a n _A oldValue, ..}

{- | Replace all occurrences of a variable within one `Type` with another `Type`,
    given the variable's  and index
-}
substituteFields :: Text -> Int -> Record s -> Type s -> Type s
substituteFields ρ0 n r@(Fields kτs ρ1) type_ =
    case type_ of
        VariableType{..} ->
            VariableType{..}
        UnsolvedType{..} ->
            UnsolvedType{..}
        Exists{type_ = oldType, ..} -> Exists{type_ = newType, ..}
          where
            newType = substituteFields ρ0 n' r oldType

            n'
                | ρ0 == name && domain == Domain.Fields = n + 1
                | otherwise = n
        Forall{type_ = oldType, ..} -> Forall{type_ = newType, ..}
          where
            newType = substituteFields ρ0 n' r oldType

            n'
                | ρ0 == name && domain == Domain.Fields = n + 1
                | otherwise = n
        Function{input = oldInput, output = oldOutput, ..} ->
            Function{input = newInput, output = newOutput, ..}
          where
            newInput = substituteFields ρ0 n r oldInput

            newOutput = substituteFields ρ0 n r oldOutput
        Optional{type_ = oldType, ..} -> Optional{type_ = newType, ..}
          where
            newType = substituteFields ρ0 n r oldType
        List{type_ = oldType, ..} -> List{type_ = newType, ..}
          where
            newType = substituteFields ρ0 n r oldType
        Tuple{types = oldTypes, ..} ->
            Tuple{types = fmap (substituteFields ρ0 n r) oldTypes, ..}
        Record{fields = Fields kAs0 ρ, ..}
            | VariableFields ρ0 == ρ && n == 0 ->
                Record{fields = Fields (map (second (substituteFields ρ0 n r)) kAs1) ρ1, ..}
            | otherwise ->
                Record{fields = Fields (map (second (substituteFields ρ0 n r)) kAs0) ρ, ..}
          where
            kAs1 = kAs0 <> map (second (fmap (const location))) kτs
        Union{alternatives = Alternatives kAs ρ, ..} ->
            Union{alternatives = Alternatives (map (second (substituteFields ρ0 n r)) kAs) ρ, ..}
        Scalar{..} ->
            Scalar{..}
        Custom{typeArguments = oldTypeArguments, ..} ->
            Custom{typeArguments = fmap (substituteFields ρ0 n r) oldTypeArguments, ..}
        Alias{value = oldType, ..} -> Alias{value = newType, ..}
          where
            newType = substituteFields ρ0 n r oldType

{- | Replace all occurrences of a variable within one `Type` with another `Type`,
    given the variable's  and index
-}
substituteAlternatives :: Text -> Int -> Union s -> Type s -> Type s
substituteAlternatives ρ0 n r@(Alternatives kτs ρ1) type_ =
    case type_ of
        VariableType{..} ->
            VariableType{..}
        UnsolvedType{..} ->
            UnsolvedType{..}
        Exists{type_ = oldType, ..} -> Exists{type_ = newType, ..}
          where
            newType = substituteAlternatives ρ0 n' r oldType

            n'
                | ρ0 == name && domain == Domain.Alternatives = n + 1
                | otherwise = n
        Forall{type_ = oldType, ..} -> Forall{type_ = newType, ..}
          where
            newType = substituteAlternatives ρ0 n' r oldType

            n'
                | ρ0 == name && domain == Domain.Alternatives = n + 1
                | otherwise = n
        Function{input = oldInput, output = oldOutput, ..} ->
            Function{input = newInput, output = newOutput, ..}
          where
            newInput = substituteAlternatives ρ0 n r oldInput

            newOutput = substituteAlternatives ρ0 n r oldOutput
        Optional{type_ = oldType, ..} -> Optional{type_ = newType, ..}
          where
            newType = substituteAlternatives ρ0 n r oldType
        List{type_ = oldType, ..} -> List{type_ = newType, ..}
          where
            newType = substituteAlternatives ρ0 n r oldType
        Tuple{types = oldFields, ..} -> Tuple{types = newFields, ..}
          where
            newFields = fmap (substituteAlternatives ρ0 n r) oldFields
        Record{fields = Fields kAs ρ, ..} ->
            Record{fields = Fields (map (second (substituteAlternatives ρ0 n r)) kAs) ρ, ..}
        Union{alternatives = Alternatives kAs0 ρ, ..}
            | Monotype.VariableAlternatives ρ0 == ρ && n == 0 ->
                Union{alternatives = Alternatives (map (second (substituteAlternatives ρ0 n r)) kAs1) ρ1, ..}
            | otherwise ->
                Union{alternatives = Alternatives (map (second (substituteAlternatives ρ0 n r)) kAs0) ρ, ..}
          where
            kAs1 = kAs0 <> map (second (fmap (const location))) kτs
        Scalar{..} ->
            Scalar{..}
        Custom{typeArguments = oldTypeArguments, ..} ->
            Custom{typeArguments = fmap (substituteAlternatives ρ0 n r) oldTypeArguments, ..}
        Alias{value = oldType, ..} -> Alias{value = newType, ..}
          where
            newType = substituteAlternatives ρ0 n r oldType

{- | Count how many times the given `Existential` `Type` variable appears within
    a `Type`
-}
typeFreeIn :: Existential Monotype -> Type s -> Bool
typeFreeIn unsolved =
    Lens.has
        ( Lens.cosmos
            . _As @"UnsolvedType"
            . the @2
            . Lens.only unsolved
        )

{- | Count how many times the given `Existential` t`Monotype.Record` variable
    appears within a `Type`
-}
fieldsFreeIn :: Existential Monotype.Record -> Type s -> Bool
fieldsFreeIn unsolved =
    Lens.has
        ( Lens.cosmos
            . _As @"Record"
            . the @2
            . the @2
            . _As @"UnsolvedFields"
            . Lens.only unsolved
        )

{- | Count how many times the given `Existential` t`Monotype.Union` variable
    appears within a `Type`
-}
alternativesFreeIn :: Existential Monotype.Union -> Type s -> Bool
alternativesFreeIn unsolved =
    Lens.has
        ( Lens.cosmos
            . _As @"Union"
            . the @2
            . the @2
            . _As @"UnsolvedAlternatives"
            . Lens.only unsolved
        )

data PreviousQuantifier
    = NoQuantifier
    | ForallQuantifier
    | ExistsQuantifier
    deriving (Eq)

instance StripLocation (Type SourceRegion) (Type ()) where
    stripLocation = void

instance (Show a) => Pretty (Type a) where
    pretty (Forall _ _ name domain type_) = keyword "∀" <+> varName (pretty name) <> "." <+> pretty type_
    pretty (Exists _ _ name domain type_) = "∃" <> pretty name <> "." <> pretty domain <> "." <> pretty type_
    pretty (VariableType _ name) = varName (pretty name)
    pretty (Scalar _ scalar) = typeName (pretty scalar)
    pretty (Function _ input output) = "(" <> pretty input <+> "->" <+> pretty output <> ")"
    pretty (Tuple _ (toList -> types)) = "(" <> hsep (punctuate "," (fmap pretty types)) <> ")"
    pretty (UnsolvedType{..}) = pretty existential <> "?"
    pretty Custom{..} = typeName (pretty name) <+> hsep (fmap pretty typeArguments)
    pretty Alias{..} = typeName (pretty name) <+> parens (pretty value)
    pretty (List _ type_) = "[" <> pretty type_ <> "]"
    pretty o = show o
