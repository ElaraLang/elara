{-# LANGUAGE PatternSynonyms #-}

-- | Types used by the type inference engine
module Elara.TypeInfer.Type where

import Data.Kind qualified as Kind
import Data.Map qualified as Map
import Elara.AST.Name
import Elara.Data.Pretty (Pretty (..), hsep, parens)
import Elara.Data.Pretty.Styles qualified as Style
import Elara.TypeInfer.Unique

data TypeVariable = UnificationVar UniqueTyVar | SkolemVar UniqueTyVar
    deriving (Generic, Show, Eq, Ord)

-- | A type scheme σ
data Type loc
    = Polytype !(Polytype loc)
    | Lifted (Monotype loc)
    deriving (Generic, Show, Eq, Ord)

data Polytype loc
    = Forall [UniqueTyVar] (Constraint loc) (Monotype loc)
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
    c1 <> c2 = (Conjunction c1 c2)

reduce :: Constraint a -> Constraint a
reduce (Conjunction q1 q2) = reduce1 (Conjunction (reduce q1) (reduce q2))
  where
    reduce1 (Conjunction EmptyConstraint EmptyConstraint) = EmptyConstraint
    reduce1 (Conjunction EmptyConstraint q) = q
    reduce1 (Conjunction q EmptyConstraint) = q
    reduce1 q = q
reduce q = q

instance Monoid (Constraint loc) where
    mempty = EmptyConstraint

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
      TypeVar TypeVariable
    | -- | A scalar
      Scalar Scalar
    | -- | A type constructor
      TypeConstructor (Qualified TypeName) [Monotype loc]
    | -- | A function type τ₁ → τ₂
      Function (Monotype loc) (Monotype loc)
    deriving (Generic, Show, Eq, Ord)

functionMonotypeResult :: Monotype loc -> Monotype loc
functionMonotypeResult = \case
    Function _ b -> functionMonotypeResult b
    t -> t

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
    -- When composing s1 <> s2, we need to apply s1 to all types in s2
    Substitution s1 <> Substitution s2 =
        Substitution $ (fmap (substituteAll (Substitution s1)) s2) <> s1

substitution :: (UniqueTyVar, Monotype loc) -> Substitution loc
substitution = Substitution . one

class Substitutable (a :: Kind.Type -> Kind.Type) loc where
    substitute :: UniqueTyVar -> Monotype loc -> a loc -> a loc

    substituteAll :: Eq (a loc) => Substitution loc -> a loc -> a loc
    substituteAll (Substitution s) a =
        let subst = foldr (uncurry substitute) a (Map.toList s)
         in if subst == a then a else substituteAll (Substitution s) subst

-- instance Substitutable Type where
--     substitute tv t (Forall tv' c m) = Forall tv' (substitute tv t c) (substitute tv t m)

instance Substitutable Constraint loc where
    substitute _ _ EmptyConstraint = EmptyConstraint
    substitute tv t (Conjunction c1 c2) = Conjunction (substitute tv t c1) (substitute tv t c2)
    substitute tv t (Equality m1 m2) = Equality (substitute tv t m1) (substitute tv t m2)

instance Substitutable Monotype loc where
    substitute _ _ (TypeVar (SkolemVar v)) = TypeVar (SkolemVar v)
    substitute tv t (TypeVar (UnificationVar v)) | tv == v = t
    substitute _ _ (TypeVar tv) = TypeVar tv
    substitute _ _ (Scalar s) = Scalar s
    substitute tv t (TypeConstructor dc ts) = TypeConstructor dc (substitute tv t <$> ts)
    substitute tv t (Function t1 t2) = Function (substitute tv t t1) (substitute tv t t2)

instance Substitutable Substitution loc where
    substitute tv t (Substitution s) = Substitution (Map.insert tv t s)

instance Pretty Scalar where
    pretty ScalarInt = Style.scalar "Int"
    pretty ScalarFloat = Style.scalar "Float"
    pretty ScalarString = Style.scalar "String"
    pretty ScalarChar = Style.scalar "Char"
    pretty ScalarUnit = Style.scalar "Unit"

instance Pretty loc => Pretty (Type loc) where
    pretty (Polytype poly) = pretty poly
    pretty (Lifted m) = pretty m

instance Pretty loc => Pretty (Polytype loc) where
    pretty (Forall [] c m) = pretty c <> " ⇒ " <> pretty m
    pretty (Forall tv c m) = "∀" <> pretty tv <> ". " <> pretty c <> " ⇒ " <> pretty m

instance Pretty loc => Pretty (Constraint loc) where
    pretty EmptyConstraint = Style.builtin "𝜖"
    pretty (Conjunction EmptyConstraint c) = pretty c
    pretty (Conjunction c EmptyConstraint) = pretty c
    pretty (Conjunction c1 c2) = parens (pretty c1) <> " ∧ " <> parens (pretty c2)
    pretty (Equality m1 m2) = pretty m1 <> " ∼ " <> pretty m2

instance Pretty (Monotype loc) where
    pretty (TypeVar tv) = Style.varName (pretty tv)
    pretty (Scalar s) = pretty s
    pretty (TypeConstructor dc ts) = Style.typeName (pretty dc) <> " " <> hsep (pretty <$> ts)
    pretty (Function t1 t2) = pretty t1 <> (Style.operator " → ") <> pretty t2

instance Pretty TypeVariable where
    pretty (UnificationVar tv) = pretty tv
    pretty (SkolemVar tv) = "#" <> pretty tv

instance Pretty (Substitution loc) where
    pretty (Substitution s) = pretty (fmap prettySubstitution (Map.toList s))
      where
        prettySubstitution (tv, t) = pretty tv <> " ↦ " <> pretty t
