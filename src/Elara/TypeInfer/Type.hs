{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
-- The Pretty Monotype instance kinda has to be here without big refactors
{-# OPTIONS_GHC -Wno-orphans #-}
-- TODO: remove this
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Elara.TypeInfer.Type where

import Data.Aeson (ToJSON (..), Value (String))
import Data.Containers.ListUtils (nubOrdOn)
import Data.Data (Data)
import Data.Generics.Product (the)
import Data.Generics.Sum (_As)
import Data.Text qualified as Text
import Elara.AST.Name (Qualified)
import Elara.AST.Region (Located (..), SourceRegion, unlocated)
import Elara.AST.StripLocation (StripLocation (stripLocation))
import Elara.Data.Pretty
import Elara.Data.Pretty.Styles
import Elara.TypeInfer.Domain (Domain)
import Elara.TypeInfer.Existential (Existential)
import Elara.TypeInfer.Monotype (
    Monotype,
    RemainingAlternatives (..),
    RemainingFields (..),
    Scalar (..),
 )
import Elara.TypeInfer.Monotype qualified as Monotype
import Elara.TypeInfer.Unique
import Optics (hasn't)
import Optics qualified as Lens
import Prettyprinter qualified as Pretty
import Print (showPrettyUnannotated)

{- $setup

  >>> :set -XOverloadedStrings
  >>> :set -XTypeApplications
  >>> import qualified Elara.TypeInfer.Monotype as Monotype
  >>> import Elara.Data.Pretty (pretty)
  >>> import qualified Elara.TypeInfer.Domain as Domain
  >>> import Data.List.NonEmpty as NE
-}

-- | A potentially polymorphic type
data Type s
    = -- | Type variable
      --
      -- >>> pretty @(Type ()) (VariableType () "a")
      -- a
      VariableType {location :: s, name :: UniqueTyVar}
    | -- | A placeholder variable whose type has not yet been inferred
      --
      -- >>> pretty @(Type ()) (UnsolvedType () 0)
      -- a?
      UnsolvedType {location :: s, existential :: Existential Monotype}
    | -- | Existentially quantified type
      --
      -- >>> pretty @(Type ()) (Exists () () "a" Domain.Type "a")
      -- exists (a : Type) . a
      Exists {location :: s, nameLocation :: s, name :: UniqueTyVar, domain :: Domain, type_ :: Type s}
    | -- | Universally quantified type
      --
      -- >>> pretty @(Type ()) (Forall () () "a" Domain.Type "a")
      -- forall (a : Type) . a
      Forall {location :: s, nameLocation :: s, name :: UniqueTyVar, domain :: Domain, type_ :: Type s}
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
    | -- | Record type
      --
      -- >>> pretty @(Type ()) (Record () (Fields [("x", "X"), ("y", "Y")] Monotype.EmptyFields))
      -- { x: X, y: Y }
      -- >>> pretty @(Type ()) (Record () (Fields [("x", "X"), ("y", "Y")] (Monotype.UnsolvedFields 0)))
      -- { x: X, y: Y, a? }
      Record {location :: s, fields :: Record s}
    | Scalar {location :: s, scalar :: Scalar}
    | -- | A custom data type
      --
      -- >>> pretty @(Type ()) (Custom () "Maybe" ["a"])
      -- Maybe a
      Custom {location :: s, conName :: Qualified Text, typeArguments :: [Type s]}
    deriving stock (Eq, Functor, Generic, Show, Data, Ord)

class StructuralEq a where
    structuralEq :: a -> a -> Bool

instance StructuralEq s => StructuralEq [s] where
    structuralEq :: [s] -> [s] -> Bool
    structuralEq [] [] = True
    structuralEq (x : xs) (y : ys) = x `structuralEq` y && xs `structuralEq` ys
    structuralEq _ _ = False

instance StructuralEq s => StructuralEq (NonEmpty s) where
    structuralEq :: NonEmpty s -> NonEmpty s -> Bool
    structuralEq (x :| xs) (y :| ys) = x `structuralEq` y && xs `structuralEq` ys

instance StructuralEq (Type s) where
    structuralEq :: Type s -> Type s -> Bool
    structuralEq (VariableType _ name1) (VariableType _ name2) = name1 == name2
    structuralEq (UnsolvedType _ existential1) (UnsolvedType _ existential2) = existential1 == existential2
    structuralEq (Exists _ _ name1 domain1 type1) (Exists _ _ name2 domain2 type2) = name1 == name2 && domain1 == domain2 && structuralEq type1 type2
    structuralEq (Forall _ _ name1 domain1 type1) (Forall _ _ name2 domain2 type2) = name1 == name2 && domain1 == domain2 && structuralEq type1 type2
    structuralEq (Function _ input1 output1) (Function _ input2 output2) = structuralEq input1 input2 && structuralEq output1 output2
    structuralEq (Optional _ type1) (Optional _ type2) = structuralEq type1 type2
    structuralEq (Record _ fields1) (Record _ fields2) = structuralEq fields1 fields2
    structuralEq (Scalar _ scalar1) (Scalar _ scalar2) = scalar1 == scalar2
    structuralEq (Custom _ conName1 typeArguments1) (Custom _ conName2 typeArguments2) = conName1 == conName2 && typeArguments1 `structuralEq` typeArguments2
    structuralEq _ _ = False

instance StructuralEq (Record s) where
    structuralEq :: Record s -> Record s -> Bool
    structuralEq (Fields fields1 remainingFields1) (Fields fields2 remainingFields2) = fields1 `structuralEq` fields2 && remainingFields1 == remainingFields2

instance (Show c, ToJSON c) => ToJSON (Type c) where
    toJSON s = String (showPrettyUnannotated s)

instance Show s => Pretty (Type s) where
    pretty = prettyQuantifiedType

instance Plated (Type s) where
    plate = traversalVL $ \onType type_ ->
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
            Record{fields = Fields oldFieldTypes remainingFields, ..} -> do
                let onPair (field, oldType) = do
                        newType <- onType oldType
                        pure (field, newType)
                newFieldTypes <- traverse onPair oldFieldTypes
                pure Record{fields = Fields newFieldTypes remainingFields, ..}
            Scalar{..} -> pure Scalar{..}
            Custom{typeArguments = oldTypeArguments, ..} -> do
                newTypeArguments <- traverse onType oldTypeArguments
                pure Custom{typeArguments = newTypeArguments, ..}

-- | A potentially polymorphic record type
data Record s = Fields [(UniqueTyVar, Type s)] RemainingFields
    deriving stock (Eq, Functor, Generic, Show, Data, Ord)

instance (Show s, ToJSON s) => ToJSON (Record s)

instance Show s => Pretty (Record s) where
    pretty = prettyRecordType

instance StructuralEq (UniqueTyVar, Type s) where
    structuralEq :: (UniqueTyVar, Type s) -> (UniqueTyVar, Type s) -> Bool
    structuralEq (name1, type1) (name2, type2) = name1 == name2 && structuralEq type1 type2

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
        Monotype.Record (Monotype.Fields kτs ρ) ->
            Record{fields = Fields (map (second fromMonotype) kτs) ρ, ..}
        Monotype.Scalar scalar ->
            Scalar{..}
        Monotype.Custom conName typeArguments ->
            Custom{typeArguments = map fromMonotype typeArguments, ..}
  where
    location = ()

instance Pretty Monotype where
    pretty = pretty . fromMonotype

stripForAll :: Type s -> Type s
stripForAll type_ =
    case type_ of
        Forall{type_ = type_'} ->
            stripForAll type_'
        _ ->
            type_

isMonoType :: Type s -> Bool
isMonoType = hasn't (cosmosOf plate % _As @"Forall")

functionTypeArgs :: Type s -> [Type s]
functionTypeArgs (stripForAll -> Function{..}) = input : functionTypeArgs output
functionTypeArgs _ = []

functionTypeReturn :: Type s -> Type s
functionTypeReturn (stripForAll -> Function{..}) = functionTypeReturn output
functionTypeReturn o = o

replaceQuantified :: Type s -> Type s -> Type s
replaceQuantified (Forall{..}) x = Forall{type_ = replaceQuantified type_ x, ..}
replaceQuantified _ y = y

instantiate :: Type s -> Type s -> Type s
Forall{name, type_} `instantiate` typeArgument = substituteType name typeArgument type_
type_ `instantiate` _ = type_

{- | In the inference algorithm, we check if a type application needs to be inserted in the 'check' function
For example, suppose we are checking that @id : forall a. a -> a@ is a subtype of @List Integer -> x?@
We know this is true if we instantiate @x?@ with @List Integer@ and instantiate @a@ with @List Integer@,
thus this creates a type application - @id \@(List Integer)@. This function matches between the two arguments
to determine the type of the type application, as naively we would instantiate it with @List Integer -> List Integer@,
which is actually incorrect.
-}
applicableTyApp :: Show s => Type s -> Type s -> [Type s]
-- If x and y are the same, there's no instantiation to be done
applicableTyApp x y | x `structuralEq` y = []
applicableTyApp _ (UnsolvedType{}) = []
-- If x is forall a. a, and y is x, then we need to instantiate x with a
-- eg `def y : Int; let y = undefined` -> `def y : Int; let y = undefined @Int`
applicableTyApp (Forall{name, type_ = VariableType{name = n}}) y | name == n = [y]
-- If x is forall a. a -> _, and y is c -> _, then we need to instantiate x with c
applicableTyApp
    (Forall{type_ = Function{input = VariableType{}}})
    Function{input = sIn} = [sIn]
applicableTyApp a@(Forall{name, type_, ..}) Forall{name = name2, type_ = type2} =
    -- Instantiating forall x. y with forall z. y[x := z] doesn't create any instantiations
    -- in other words, if the first forall is just a copy of the second with a different name, then we don't need to instantiate
    if type_ `structuralEq` substituteType name2 (VariableType{name = name, ..}) type2
        then applicableTyApp type_ (substituteType name2 (VariableType{name = name, ..}) type2)
        else applicableTyApp a type2
-- If x is forall a. X a and y is X b, then we need to instantiate x with b
-- applicableTyApp (Forall{type_ = Custom{typeArguments = [VariableType{..}]}}) Custom{typeArguments = [y]} =
--     [y]
applicableTyApp x y = []

freeTypeVars :: Type SourceRegion -> [Located UniqueTyVar]
-- todo: make this check for foralls rather than assuming they're all free
freeTypeVars t = nubOrdOn (view unlocated) (concatMapOf cosmos names t)
  where
    names = \case
        VariableType sr l -> [Located sr l]
        _ -> []

occurs :: UniqueTyVar -> Type SourceRegion -> Bool
occurs tv t = tv `elem` (view unlocated <$> freeTypeVars t)

{- | Substitute a `Type` by replacing all occurrences of the given unsolved
   variable with a `Monotype`
-}
solveType :: Existential Monotype -> Monotype -> Type s -> Type s
solveType unsolved monotype = transform transformType
  where
    transformType UnsolvedType{..}
        | unsolved == existential = location <$ fromMonotype monotype
    transformType type_ =
        type_

{- | Substitute a `Type` by replacing all occurrences of the given unsolved
   fields variable with a t`Monotype.Record`
-}
solveFields ::
    Existential Monotype.Record -> Monotype.Record -> Type s -> Type s
solveFields unsolved (Monotype.Fields fieldMonotypes fields) =
    transform transformType
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

{- | Replace all occurrences of a variable within one `Type` with another `Type`,
   given the variable's label and index
-}
substituteType :: UniqueTyVar -> Type s -> Type s -> Type s
substituteType a _A type_ =
    case type_ of
        VariableType{..}
            | a == name -> _A
            | otherwise -> VariableType{..}
        UnsolvedType{..} ->
            UnsolvedType{..}
        Exists{type_ = oldType, ..} -> Exists{type_ = newType, ..}
          where
            newType = substituteType a _A oldType
        Forall{type_ = oldType, ..} -> Forall{type_ = newType, ..}
          where
            newType = substituteType a _A oldType
        Function{input = oldInput, output = oldOutput, ..} ->
            Function{input = newInput, output = newOutput, ..}
          where
            newInput = substituteType a _A oldInput

            newOutput = substituteType a _A oldOutput
        Optional{type_ = oldType, ..} -> Optional{type_ = newType, ..}
          where
            newType = substituteType a _A oldType
        Record{fields = Fields kAs ρ, ..} ->
            Record{fields = Fields (map (second (substituteType a _A)) kAs) ρ, ..}
        Scalar{..} ->
            Scalar{..}
        Custom{typeArguments = oldTypeArguments, ..} ->
            Custom{typeArguments = fmap (substituteType a _A) oldTypeArguments, ..}

{- | Replace all occurrences of a variable within one `Type` with another `Type`,
   given the variable's label and index
-}
substituteFields :: UniqueTyVar -> Record s -> Type s -> Type s
substituteFields ρ0 r@(Fields kτs ρ1) type_ =
    case type_ of
        VariableType{..} ->
            VariableType{..}
        UnsolvedType{..} ->
            UnsolvedType{..}
        Exists{type_ = oldType, ..} -> Exists{type_ = newType, ..}
          where
            newType = substituteFields ρ0 r oldType
        Forall{type_ = oldType, ..} -> Forall{type_ = newType, ..}
          where
            newType = substituteFields ρ0 r oldType
        Function{input = oldInput, output = oldOutput, ..} ->
            Function{input = newInput, output = newOutput, ..}
          where
            newInput = substituteFields ρ0 r oldInput

            newOutput = substituteFields ρ0 r oldOutput
        Optional{type_ = oldType, ..} -> Optional{type_ = newType, ..}
          where
            newType = substituteFields ρ0 r oldType
        Record{fields = Fields kAs0 ρ, ..}
            | VariableFields ρ0 == ρ ->
                Record{fields = Fields (map (second (substituteFields ρ0 r)) kAs1) ρ1, ..}
            | otherwise ->
                Record{fields = Fields (map (second (substituteFields ρ0 r)) kAs0) ρ, ..}
          where
            kAs1 = kAs0 <> map (second (fmap (const location))) kτs
        Scalar{..} ->
            Scalar{..}
        Custom{typeArguments = oldTypeArguments, ..} ->
            Custom{typeArguments = fmap (substituteFields ρ0 r) oldTypeArguments, ..}

{- | Count how many times the given `Existential` `Type` variable appears within
   a `Type`
-}
typeFreeIn :: Existential Monotype -> Type s -> Bool
typeFreeIn unsolved =
    Lens.has
        ( cosmos
            % _As @"UnsolvedType"
            % the @2
            % Lens.only unsolved
        )

{- | Count how many times the given `Existential` t`Monotype.Record` variable
   appears within a `Type`
-}
fieldsFreeIn :: Existential Monotype.Record -> Type s -> Bool
fieldsFreeIn unsolved =
    Lens.has
        ( cosmos
            % _As @"Record"
            % the @2
            % the @2
            % _As @"UnsolvedFields"
            % Lens.only unsolved
        )

data PreviousQuantifier
    = NoQuantifier
    | ForallQuantifier
    | ExistsQuantifier
    deriving (Eq)

instance StripLocation a b => StripLocation (Type a) (Type b) where
    stripLocation = fmap (stripLocation @a @b)

prettyQuantifiedType :: Show s => Type s -> Doc AnsiStyle
prettyQuantifiedType type0
    | isQuantified type0 = Pretty.group (Pretty.flatAlt long short)
    | otherwise = prettyFunctionType type0
  where
    isQuantified Forall{} = True
    isQuantified Exists{} = True
    isQuantified _ = False

    short = prettyShort NoQuantifier type0

    long = Pretty.align (prettyLong type0)

    prettyShort quantifier Forall{..} =
        prefix
            <> label (pretty name)
            <> " "
            <> prettyShort ForallQuantifier type_
      where
        prefix =
            case quantifier of
                NoQuantifier -> keyword "forall" <> " "
                ExistsQuantifier -> punctuation "." <> " " <> keyword "forall" <> " "
                ForallQuantifier -> ""
    prettyShort quantifier Exists{..} =
        prefix
            <> label (pretty name)
            <> " "
            <> prettyShort ExistsQuantifier type_
      where
        prefix =
            case quantifier of
                NoQuantifier -> keyword "exists" <> " "
                ExistsQuantifier -> ""
                ForallQuantifier -> punctuation "." <> " " <> keyword "exists" <> " "
    prettyShort _quantifier _A =
        punctuation "." <> " " <> prettyFunctionType _A

    prettyLong Forall{..} =
        keyword "forall"
            <> " "
            <> label (pretty name)
            <> punctuation "."
            <> Pretty.hardline
            <> prettyLong type_
    prettyLong Exists{..} =
        keyword "exists"
            <> " "
            <> punctuation "("
            <> label (pretty name)
            <> " "
            <> punctuation ":"
            <> " "
            <> pretty domain
            <> punctuation ")"
            <> " "
            <> punctuation "."
            <> Pretty.hardline
            <> prettyLong type_
    prettyLong _A =
        "  " <> prettyFunctionType _A

prettyFunctionType :: Show s => Type s -> Doc AnsiStyle
prettyFunctionType type_@Function{} = Pretty.group (Pretty.flatAlt long short)
  where
    long = Pretty.align (prettyLong type_)

    short = prettyShort type_

    prettyShort Function{..} =
        prettyApplicationType input
            <> " "
            <> punctuation "->"
            <> " "
            <> prettyShort output
    prettyShort _A =
        prettyApplicationType _A

    prettyLong Function{..} =
        prettyApplicationType input
            <> " "
            <> punctuation "->"
            <> Pretty.hardline
            <> prettyLong output
    prettyLong _A =
        "  " <> prettyApplicationType _A
prettyFunctionType other =
    prettyApplicationType other

prettyApplicationType :: Show s => Type s -> Doc AnsiStyle
prettyApplicationType Optional{..} = Pretty.group (Pretty.flatAlt long short)
  where
    short = builtin "Optional" <> " " <> prettyPrimitiveType type_

    long =
        Pretty.align
            ( builtin "Optional"
                <> Pretty.hardline
                <> "  "
                <> prettyPrimitiveType type_
            )
-- prettyApplicationType List{..} = Pretty.group (Pretty.flatAlt long short)
--   where
--     short = builtin "List" <> " " <> prettyPrimitiveType type_

--     long =
--         Pretty.align
--             ( builtin "List"
--                 <> Pretty.hardline
--                 <> "  "
--                 <> prettyPrimitiveType type_
--             )
prettyApplicationType other =
    prettyPrimitiveType other

prettyPrimitiveType :: Show s => Type s -> Doc AnsiStyle
prettyPrimitiveType VariableType{..} =
    label (pretty name)
prettyPrimitiveType UnsolvedType{..} =
    label (pretty existential <> "?")
prettyPrimitiveType Record{..} =
    prettyRecordType fields
prettyPrimitiveType Scalar{..} =
    Elara.Data.Pretty.Styles.scalar (pretty scalar)
prettyPrimitiveType Custom{..} =
    if null typeArguments
        then label (pretty conName)
        else
            punctuation "("
                <> label (pretty conName)
                <> " "
                <> Pretty.group (Pretty.flatAlt long short)
                <> punctuation ")"
  where
    short =
        foldMap (\t -> prettyQuantifiedType t <> " ") typeArguments

    long =
        Pretty.align
            ( foldMap (\t -> prettyQuantifiedType t <> Pretty.hardline) typeArguments
            )
-- prettyPrimitiveType Tuple{..} =
--     Pretty.group (Pretty.flatAlt long short)
--   where
--     short =
--         punctuation "("
--             <> prettyQuantifiedType (head tupleArguments)
--             <> foldMap (\t -> punctuation "," <> " " <> prettyQuantifiedType t) (tail tupleArguments)
--             <> punctuation ")"

--     long =
--         Pretty.align
--             ( punctuation "("
--                 <> " "
--                 <> prettyQuantifiedType (head tupleArguments)
--                 <> foldMap (\t -> punctuation "," <> " " <> prettyQuantifiedType t) (tail tupleArguments)
--                 <> Pretty.hardline
--                 <> punctuation ")"
--             )
prettyPrimitiveType other =
    Pretty.group (Pretty.flatAlt long short)
  where
    short = punctuation "(" <> prettyQuantifiedType other <> punctuation ")"

    long =
        Pretty.align
            ( punctuation "("
                <> " "
                <> prettyQuantifiedType other
                <> Pretty.hardline
                <> punctuation ")"
            )

prettyRecordType :: Show s => Record s -> Doc AnsiStyle
prettyRecordType (Fields [] fields) =
    punctuation "{"
        <> ( case fields of
                EmptyFields -> " "
                UnsolvedFields ρ -> " " <> label (pretty ρ <> "?") <> " "
                VariableFields ρ -> " " <> label (pretty ρ) <> " "
           )
        <> punctuation "}"
prettyRecordType (Fields (keyType : keyTypes) fields) =
    Pretty.group (Pretty.flatAlt long short)
  where
    short =
        punctuation "{"
            <> " "
            <> prettyShortFieldType keyType
            <> foldMap (\ft -> punctuation "," <> " " <> prettyShortFieldType ft) keyTypes
            <> ( case fields of
                    EmptyFields ->
                        mempty
                    UnsolvedFields ρ ->
                        punctuation "," <> " " <> label (pretty ρ <> "?")
                    VariableFields ρ ->
                        punctuation "," <> " " <> label (pretty ρ)
               )
            <> " "
            <> punctuation "}"

    long =
        Pretty.align
            ( punctuation "{"
                <> " "
                <> prettyLongFieldType keyType
                <> foldMap (\ft -> punctuation "," <> " " <> prettyLongFieldType ft) keyTypes
                <> case fields of
                    EmptyFields ->
                        punctuation "}"
                    UnsolvedFields ρ ->
                        punctuation ","
                            <> " "
                            <> label (pretty ρ <> "?")
                            <> Pretty.hardline
                            <> punctuation "}"
                    VariableFields ρ ->
                        punctuation ","
                            <> " "
                            <> label (pretty ρ)
                            <> Pretty.hardline
                            <> punctuation "}"
            )

    prettyShortFieldType :: Show s => (UniqueTyVar, Type s) -> Doc AnsiStyle
    prettyShortFieldType (key, type_) =
        prettyRecordLabel False key
            <> operator ":"
            <> " "
            <> prettyQuantifiedType type_

    prettyLongFieldType :: Show s => (UniqueTyVar, Type s) -> Doc AnsiStyle
    prettyLongFieldType (key, type_) =
        prettyRecordLabel False key
            <> operator ":"
            <> Pretty.group (Pretty.flatAlt (Pretty.hardline <> "    ") " ")
            <> prettyQuantifiedType type_
            <> Pretty.hardline

-- | Pretty-print a @Text@ literal
prettyTextLiteral :: Text -> Doc AnsiStyle
prettyTextLiteral text =
    "\""
        <> ( pretty
                . Text.replace "\"" "\\\""
                . Text.replace "\b" "\\b"
                . Text.replace "\f" "\\f"
                . Text.replace "\n" "\\n"
                . Text.replace "\r" "\\r"
                . Text.replace "\t" "\\t"
                . Text.replace "\\" "\\\\"
           )
            text
        <> "\""

-- | Pretty-print a @Text@ literal
prettyQuotedAlternative :: Text -> Doc AnsiStyle
prettyQuotedAlternative text =
    "'"
        <> ( pretty
                . Text.replace "'" "\\\'"
                . Text.replace "\b" "\\b"
                . Text.replace "\f" "\\f"
                . Text.replace "\n" "\\n"
                . Text.replace "\r" "\\r"
                . Text.replace "\t" "\\t"
                . Text.replace "\\" "\\\\"
           )
            text
        <> "'"

-- | Pretty-print a record label
prettyRecordLabel ::
    -- | Always quote the label if `True`
    --
    -- This is mainly set to `True` when pretty-printing records so that the
    -- output is valid JSON
    Bool ->
    UniqueTyVar ->
    Doc AnsiStyle
prettyRecordLabel alwaysQuote field
    | not alwaysQuote =
        label (pretty field)
    | otherwise =
        label (pretty field)

-- | Pretty-print an alternative label
prettyAlternativeLabel ::
    UniqueTyVar ->
    Doc AnsiStyle
prettyAlternativeLabel alternative =
    label (pretty alternative)
