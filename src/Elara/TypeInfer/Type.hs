{-# LANGUAGE PatternSynonyms #-}

-- | Types used by the type inference engine
module Elara.TypeInfer.Type where

import Data.Kind qualified as Kind
import Data.Map qualified as Map
import Elara.AST.Name
import Elara.Data.Pretty (Pretty (..), hsep)
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

newtype Substitution loc
    = Substitution
        (Map UniqueTyVar (Monotype loc))
    deriving newtype (Monoid)
    deriving stock (Eq, Show)

instance Semigroup (Substitution loc) where
    Substitution s1 <> Substitution s2 =
        Substitution $
            Map.union (fmap (substituteAll (Substitution s1)) s2) s1

substitution :: (UniqueTyVar, Monotype loc) -> Substitution loc
substitution = Substitution . one

class Substitutable (a :: k -> Kind.Type) where
    substitute :: UniqueTyVar -> Monotype loc -> a loc -> a loc
    substituteAll :: Substitution loc -> a loc -> a loc
    substituteAll (Substitution s) a =
        foldr (\(tv, t) a' -> substitute tv t a') a (Map.toList s)

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
    substitute tv t (Function t1 t2) = Function (substitute tv t t1) (substitute tv t t2)

instance Pretty Scalar where
    pretty ScalarInt = "Int"
    pretty ScalarFloat = "Float"
    pretty ScalarString = "String"
    pretty ScalarChar = "Char"
    pretty ScalarUnit = "Unit"

instance Pretty loc => Pretty (Type loc) where
    pretty (Forall tv c m) = "∀" <> pretty tv <> ". " <> pretty c <> " ⇒ " <> pretty m

instance Pretty loc => Pretty (Constraint loc) where
    pretty EmptyConstraint = "𝜖"
    pretty (Conjunction c1 c2) = pretty c1 <> " ∧ " <> pretty c2
    pretty (Equality m1 m2) = pretty m1 <> " ∼ " <> pretty m2

instance Pretty loc => Pretty (Monotype loc) where
    pretty (TypeVar tv) = pretty tv
    pretty (Scalar s) = pretty s
    pretty (TypeConstructor dc ts) = pretty dc <> " " <> hsep (pretty <$> ts)
    pretty (Function t1 t2) = pretty t1 <> " → " <> pretty t2

instance Pretty loc => Pretty (Substitution loc) where
    pretty (Substitution s) = pretty (fmap prettySubstitution (Map.toList s))
      where
        prettySubstitution (tv, t) = pretty tv <> " ↦ " <> pretty t
