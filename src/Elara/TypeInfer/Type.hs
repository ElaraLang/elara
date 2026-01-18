{-# LANGUAGE RecordWildCards #-}

-- | Types used by the type inference engine
module Elara.TypeInfer.Type (
    -- * Type Variables
    TypeVariable (..),
    UniqueTyVar,

    -- * Types
    Type (..),
    Polytype (..),
    Monotype (..),
    typeLoc,
    polytypeLoc,
    monotypeLoc,
    functionMonotypeResult,
    functionMonotypeArgs,

    -- * Constraints
    Constraint (..),
    constraintLoc,
    simpleEquality,
    equalityWithContext,
    reduce,
    emptyLocation,

    -- * Data Constructors
    DataCon,

    -- * Axiom Schemes
    AxiomScheme (..),

    -- * Substitution
    Substitution (..),
    Substitutable (..),
    substitution,
) where

import Data.Kind qualified as Kind
import Data.Map qualified as Map
import Elara.AST.Name
import Elara.AST.Region (SourceRegion, generatedSourceRegion)
import Elara.Data.Pretty (Pretty (..), hsep, parens)
import Elara.Data.Pretty.Styles qualified as Style
import Elara.TypeInfer.Context (InferenceContext)
import Elara.TypeInfer.Unique

data TypeVariable = UnificationVar UniqueTyVar | SkolemVar UniqueTyVar
    deriving (Generic, Show, Eq, Ord)

-- | A type scheme σ
data Type loc
    = Polytype !(Polytype loc)
    | Lifted (Monotype loc)
    deriving (Generic, Show, Eq, Ord)

data Polytype loc
    = Forall loc [UniqueTyVar] (Constraint loc) (Monotype loc)
    deriving (Generic, Show, Eq, Ord)

typeLoc :: Type loc -> loc
typeLoc (Polytype p) = polytypeLoc p
typeLoc (Lifted m) = monotypeLoc m

polytypeLoc :: Polytype loc -> loc
polytypeLoc (Forall loc _ _ _) = loc

-- | A constraint Q
data Constraint loc
    = -- | The empty constraint 𝜖
      EmptyConstraint loc
    | -- | The conjunction of two constraints, Q₁ ∧ Q₂
      Conjunction loc (Constraint loc) (Constraint loc)
    | {- | An equality constraint, τ₁ ∼ τ₂
      Now enriched with usage locations and inference context
      -}
      Equality
        { eqLoc :: loc
        -- ^ Where the constraint was generated
        , eqLeft :: Monotype loc
        -- ^ The left-hand side type
        , eqRight :: Monotype loc
        -- ^ The right-hand side type
        , eqLeftUsage :: loc
        -- ^ Where the left type was used/expected
        , eqRightUsage :: loc
        -- ^ Where the right type was found/provided
        , eqContext :: Maybe InferenceContext
        -- ^ Why we're comparing these types
        }
    deriving (Generic, Show, Eq, Ord)

constraintLoc :: Constraint loc -> loc
constraintLoc (EmptyConstraint loc) = loc
constraintLoc (Conjunction loc _ _) = loc
constraintLoc Equality{eqLoc = loc} = loc

{- | Smart constructor for simple equality constraints (no context)
Uses the monotype locations as usage locations
-}
simpleEquality :: loc -> Monotype loc -> Monotype loc -> Constraint loc
simpleEquality loc left right =
    Equality
        { eqLoc = loc
        , eqLeft = left
        , eqRight = right
        , eqLeftUsage = monotypeLoc left
        , eqRightUsage = monotypeLoc right
        , eqContext = Nothing
        }

-- | Smart constructor for equality constraints with context
equalityWithContext :: loc -> Monotype loc -> Monotype loc -> loc -> loc -> Maybe InferenceContext -> Constraint loc
equalityWithContext loc left right leftUsage rightUsage ctx =
    Equality
        { eqLoc = loc
        , eqLeft = left
        , eqRight = right
        , eqLeftUsage = leftUsage
        , eqRightUsage = rightUsage
        , eqContext = ctx
        }

instance Plated (Constraint loc)

emptyLocation :: SourceRegion
emptyLocation = generatedSourceRegion Nothing

instance {-# OVERLAPPING #-} Monoid (Constraint SourceRegion) where
    mempty = EmptyConstraint emptyLocation

instance (Monoid loc, Eq loc) => Monoid (Constraint loc) where
    mempty = EmptyConstraint mempty

instance (Eq loc, Semigroup loc) => Semigroup (Constraint loc) where
    (<>) :: (Eq loc, Semigroup loc, HasCallStack) => Constraint loc -> Constraint loc -> Constraint loc
    EmptyConstraint _ <> c = c
    c <> EmptyConstraint _ = c
    c1 <> c2 | c1 == c2 = c1 -- Reflexivity
    c1 <> c2 = Conjunction (constraintLoc c1) c1 c2

reduce :: Constraint a -> Constraint a
reduce (Conjunction l q1 q2) = reduce1 (Conjunction l (reduce q1) (reduce q2))
  where
    reduce1 (Conjunction l (EmptyConstraint _) (EmptyConstraint _)) = EmptyConstraint l
    reduce1 (Conjunction _ (EmptyConstraint _) q) = q
    reduce1 (Conjunction _ q (EmptyConstraint{})) = q
    reduce1 q = q
reduce q = q

-- instance Eq loc => Monoid (Constraint loc) where
--     mempty = EmptyConstraint

-- | An axiom scheme QQ
data AxiomScheme loc
    = -- | The empty axiom scheme 𝜖
      EmptyAxiomScheme
    | -- | The conjunction of two axiom schemes, QQ₁ ∧ QQ₂
      ConjunctionAxiomScheme (AxiomScheme loc) (AxiomScheme loc)
    | {- | A universal quantification of an axiom scheme, ∀α. QQ ⇒ QQ
      Practically this could be a declaration like @forall a. Eq a => Eq [a]@
      -}
      ForallAxiomScheme
        -- | Type variable to be quantified
        UniqueTyVar
        -- | Constraint for the quantified variable
        (AxiomScheme loc)
        -- | The axiom
        (AxiomScheme loc)
    deriving (Generic, Show, Eq, Ord)

-- | A monotype τ
data Monotype (loc :: Kind.Type)
    = -- | A type variable tv
      TypeVar loc TypeVariable
    | -- | A type constructor
      TypeConstructor loc (Qualified TypeName) [Monotype loc]
    | -- | A function type τ₁ → τ₂
      Function loc (Monotype loc) (Monotype loc)
    deriving (Generic, Show, Eq, Ord)

monotypeLoc :: Monotype loc -> loc
monotypeLoc (TypeVar loc _) = loc
monotypeLoc (TypeConstructor loc _ _) = loc
monotypeLoc (Function loc _ _) = loc

functionMonotypeResult :: Monotype loc -> Monotype loc
functionMonotypeResult = \case
    Function _ _ b -> functionMonotypeResult b
    t -> t

functionMonotypeArgs :: Monotype loc -> [Monotype loc]
functionMonotypeArgs = \case
    Function _ a b -> a : functionMonotypeArgs b
    _ -> []

type DataCon = Qualified TypeName

newtype Substitution loc
    = Substitution
        (Map UniqueTyVar (Monotype loc))
    deriving newtype (Monoid)
    deriving stock (Eq, Show)

instance Eq loc => Semigroup (Substitution loc) where
    -- When composing s1 <> s2, we need to apply s1 to all types in s2
    Substitution s1 <> Substitution s2 =
        Substitution $ fmap (substituteAll (Substitution s1)) s2 <> s1

substitution :: (UniqueTyVar, Monotype loc) -> Substitution loc
substitution = Substitution . one

class Substitutable (a :: Kind.Type -> Kind.Type) loc where
    substitute :: UniqueTyVar -> Monotype loc -> a loc -> a loc

    -- | Apply an entire substitution to a type, recursively until a fixed point is reached
    substituteAll :: Eq (a loc) => Substitution loc -> a loc -> a loc

-- we keep this default impl disabled by default to encourage writing custom efficient implementations
-- substituteAll (Substitution s) a = let subst = foldr (uncurry substitute) a (Map.toList s) in if subst == a then a else defaultSubstituteAll (Substitution s) subst -- recursively apply until we reach a fixed point

instance Eq loc => Substitutable Constraint loc where
    substitute _ _ (EmptyConstraint l) = EmptyConstraint l
    substitute tv t (Conjunction l c1 c2) = Conjunction l (substitute tv t c1) (substitute tv t c2)
    substitute tv t Equality{..} =
        Equality
            { eqLoc = eqLoc
            , eqLeft = substitute tv t eqLeft
            , eqRight = substitute tv t eqRight
            , eqLeftUsage = eqLeftUsage
            , eqRightUsage = eqRightUsage
            , eqContext = eqContext
            }

    substituteAll _ (EmptyConstraint l) = EmptyConstraint l
    substituteAll s (Conjunction l c1 c2) = Conjunction l (substituteAll s c1) (substituteAll s c2)
    substituteAll s Equality{..} =
        Equality
            { eqLoc = eqLoc
            , eqLeft = substituteAll s eqLeft
            , eqRight = substituteAll s eqRight
            , eqLeftUsage = eqLeftUsage
            , eqRightUsage = eqRightUsage
            , eqContext = eqContext
            }

instance Substitutable Monotype loc where
    substitute _ _ (TypeVar loc (SkolemVar v)) = TypeVar loc (SkolemVar v)
    substitute tv t (TypeVar _ (UnificationVar v)) | tv == v = t
    substitute _ _ (TypeVar loc tv) = TypeVar loc tv
    substitute tv t (TypeConstructor loc dc ts) = TypeConstructor loc dc (substitute tv t <$> ts)
    substitute tv t (Function loc t1 t2) = Function loc (substitute tv t t1) (substitute tv t t2)

    substituteAll (Substitution m) t@(TypeVar loc tv) = case tv of
        SkolemVar v -> TypeVar loc (SkolemVar v) -- skolems are rigid
        UnificationVar v ->
            -- make sure we recursively apply substitutions until we reach a fixed point
            let r = case Map.lookup v m of
                    Just t' -> t'
                    Nothing -> TypeVar loc (UnificationVar v)
             in if r == t then t else substituteAll (Substitution m) r
    substituteAll s (TypeConstructor loc dc ts) = TypeConstructor loc dc (substituteAll s <$> ts)
    substituteAll s (Function loc t1 t2) = Function loc (substituteAll s t1) (substituteAll s t2)

instance Substitutable Substitution loc where
    substitute tv t (Substitution s) = Substitution (Map.insert tv t s)

instance Pretty loc => Pretty (Type loc) where
    pretty (Polytype poly) = pretty poly
    pretty (Lifted m) = pretty m

instance Pretty loc => Pretty (Polytype loc) where
    pretty (Forall _ tv c m) = "∀" <> hsep (pretty <$> tv) <> ". " <> pretty c <> " ⇒ " <> pretty m

instance Pretty loc => Pretty (Constraint loc) where
    pretty EmptyConstraint{} = Style.builtin "𝜖"
    pretty (Conjunction _ EmptyConstraint{} c) = pretty c
    pretty (Conjunction _ c EmptyConstraint{}) = pretty c
    pretty (Conjunction _ c1 c2) = parens (pretty c1) <> " ∧ " <> parens (pretty c2)
    pretty Equality{eqLeft = m1, eqRight = m2} = pretty m1 <> " ∼ " <> pretty m2

instance Pretty (Monotype loc) where
    pretty (TypeVar _ tv) = Style.varName (pretty tv)
    pretty (TypeConstructor _ dc ts) = Style.typeName (pretty dc) <> " " <> hsep (pretty <$> ts)
    pretty (Function _ f@(Function{}) t3) = parens (pretty f) <> Style.operator " → " <> pretty t3
    pretty (Function _ t1 t2) = pretty t1 <> Style.operator " → " <> pretty t2

instance Pretty TypeVariable where
    pretty (UnificationVar tv) = pretty tv
    pretty (SkolemVar tv) = "#" <> pretty tv

instance Pretty (Substitution loc) where
    pretty (Substitution s) = pretty (fmap prettySubstitution (Map.toList s))
      where
        prettySubstitution (tv, t) = pretty tv <> " ↦ " <> pretty t
