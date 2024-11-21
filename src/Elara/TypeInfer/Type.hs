-- | Types used by the type inference engine
module Elara.TypeInfer.Type where

import Elara.AST.Name
import Elara.AST.VarRef (UnlocatedVarRef, VarRef)
import Elara.TypeInfer.Unique
import Prelude hiding (Constraint)

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

-- | An axiom scheme QQ
data AxiomScheme loc
    = -- | The empty axiom scheme 𝜖
      EmptyAxiomScheme
    | -- | The conjunction of two axiom schemes, QQ₁ ∧ QQ₂
      ConjunctionAxiomScheme (AxiomScheme loc) (AxiomScheme loc)
    | -- | A universal quantification of an axiom scheme, ∀α. QQ ⇒ QQ
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
    deriving (Generic, Show, Eq, Ord)

type DataCon = Qualified TypeName

class Substitutable a where
    substitute :: UniqueTyVar -> Monotype loc -> a loc -> a loc

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
