-- | Types used by the type inference engine
module Elara.TypeInfer.Type where

import Data.Kind qualified as Kind
import Elara.AST.Name
import Elara.TypeInfer.Unique

-- | A type scheme σ
data Type loc
    = Forall UniqueTyVar (Constraint loc) (Monotype loc)
    deriving (Generic, Show, Eq, Ord)

-- | A constraint Q
data Constraint loc
    = -- | The empty constraint 𝜖
      EmptyConstraint
    | -- | The conjunction of two constraints, Q₁ ∧ Q₂
      Conjunction (Constraint loc) (Constraint loc)
    | -- | An equality constraint, τ₁ ∼ τ₂
      Equality (Monotype loc) (Monotype loc)
    deriving (Generic, Show, Eq, Ord)

instance Semigroup (Constraint loc) where
    EmptyConstraint <> c = c
    c <> EmptyConstraint = c
    c1 <> c2 | c1 == c2 = c1 -- Reflexivity
    c1 <> c2 = Conjunction c1 c2

instance Monoid (Constraint loc) where
    mempty = EmptyConstraint

-- | An axiom scheme QQ
data AxiomScheme loc
    = -- | The empty axiom scheme 𝜖
      EmptyAxiomScheme
    | -- | The conjunction of two axiom schemes, QQ₁ ∧ QQ₂
      ConjunctionAxiomScheme (AxiomScheme loc) (AxiomScheme loc)
    | -- | A universal quantification of an axiom scheme, ∀α. QQ ⇒ QQ
      -- Practically this could be a declaration like @forall a. Eq a => Eq [a]@
      ForallAxiomScheme
        -- | Type variable to be quantified
        UniqueTyVar
        -- | Constraint for the quantified variable
        (AxiomScheme loc)
        -- | The axiom
        (AxiomScheme loc)
    deriving (Generic, Show, Eq, Ord)

-- | A monotype τ
data Monotype loc
    = -- | A type variable tv
      TypeVar UniqueTyVar
    | -- | A scalar
      Scalar Scalar
    | -- | A type constructor
      TypeConstructor (Qualified TypeName) [Monotype loc]
    | -- | A function type τ₁ → τ₂
      Function (Monotype loc) (Monotype loc)
    deriving (Generic, Show, Eq, Ord)

-- | A scalar type
data Scalar
    = ScalarInt
    | ScalarFloat
    | ScalarString
    | ScalarChar
    | ScalarUnit
    deriving (Generic, Show, Eq, Ord, Enum, Bounded)

type DataCon = Qualified TypeName

newtype Substitution loc = Substitution [(UniqueTyVar, Monotype loc)]
    deriving newtype (Semigroup, Monoid)
    deriving stock (Eq, Show)

class Substitutable (a :: k -> Kind.Type) where
    substitute :: UniqueTyVar -> Monotype loc -> a loc -> a loc
    substituteAll :: Substitution loc -> a loc -> a loc
    substituteAll (Substitution s) a = foldl' (\a (tv, t) -> substitute tv t a) a s

instance Substitutable Type where
    substitute tv t (Forall tv' c m) = Forall tv' (substitute tv t c) (substitute tv t m)

instance Substitutable Constraint where
    substitute tv t EmptyConstraint = EmptyConstraint
    substitute tv t (Conjunction c1 c2) = Conjunction (substitute tv t c1) (substitute tv t c2)
    substitute tv t (Equality m1 m2) = Equality (substitute tv t m1) (substitute tv t m2)

instance Substitutable Monotype where
    substitute tv t (TypeVar tv') = if tv == tv' then t else TypeVar tv'
    substitute tv t (Scalar s) = Scalar s
    substitute tv t (TypeConstructor dc ts) = TypeConstructor dc (substitute tv t <$> ts)
