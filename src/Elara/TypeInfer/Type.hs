-- | Types used by the type inference engine
module Elara.TypeInfer.Type where

import Control.Lens (cons)
import Data.Kind qualified as Kind
import Data.Map qualified as Map
import Elara.AST.Name
import Elara.AST.Region (SourceRegion, generatedSourceRegion, spanningRegion)
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
    | -- | An equality constraint, τ₁ ∼ τ₂
      Equality loc (Monotype loc) (Monotype loc)
    deriving (Generic, Show, Eq, Ord)

constraintLoc :: Constraint loc -> loc
constraintLoc (EmptyConstraint loc) = loc
constraintLoc (Conjunction loc _ _) = loc
constraintLoc (Equality loc _ _) = loc

instance Plated (Constraint loc)

emptyLocation :: SourceRegion
emptyLocation = generatedSourceRegion Nothing

instance Monoid (Constraint SourceRegion) where
    mempty = EmptyConstraint emptyLocation

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
    | -- | A scalar
      Scalar loc Scalar
    | -- | A type constructor
      TypeConstructor loc (Qualified TypeName) [Monotype loc]
    | -- | A function type τ₁ → τ₂
      Function loc (Monotype loc) (Monotype loc)
    deriving (Generic, Show, Eq, Ord)

monotypeLoc :: Monotype loc -> loc
monotypeLoc (TypeVar loc _) = loc
monotypeLoc (Scalar loc _) = loc
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

instance Eq loc => Semigroup (Substitution loc) where
    -- When composing s1 <> s2, we need to apply s1 to all types in s2
    Substitution s1 <> Substitution s2 =
        Substitution $ fmap (substituteAll (Substitution s1)) s2 <> s1

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
    substitute _ _ (EmptyConstraint l) = EmptyConstraint l
    substitute tv t (Conjunction l c1 c2) = Conjunction l (substitute tv t c1) (substitute tv t c2)
    substitute tv t (Equality l m1 m2) = Equality l (substitute tv t m1) (substitute tv t m2)

instance Substitutable Monotype loc where
    substitute _ _ (TypeVar loc (SkolemVar v)) = TypeVar loc (SkolemVar v)
    substitute tv t (TypeVar _ (UnificationVar v)) | tv == v = t
    substitute _ _ (TypeVar loc tv) = TypeVar loc tv
    substitute _ _ (Scalar loc s) = Scalar loc s
    substitute tv t (TypeConstructor loc dc ts) = TypeConstructor loc dc (substitute tv t <$> ts)
    substitute tv t (Function loc t1 t2) = Function loc (substitute tv t t1) (substitute tv t t2)

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
    pretty (Forall _ tv c m) = "∀" <> hsep (pretty <$> tv) <> ". " <> pretty c <> " ⇒ " <> pretty m

instance Pretty loc => Pretty (Constraint loc) where
    pretty EmptyConstraint{} = Style.builtin "𝜖"
    pretty (Conjunction _ EmptyConstraint{} c) = pretty c
    pretty (Conjunction _ c EmptyConstraint{}) = pretty c
    pretty (Conjunction _ c1 c2) = parens (pretty c1) <> " ∧ " <> parens (pretty c2)
    pretty (Equality _ m1 m2) = pretty m1 <> " ∼ " <> pretty m2

instance Pretty (Monotype loc) where
    pretty (TypeVar _ tv) = Style.varName (pretty tv)
    pretty (Scalar _ s) = pretty s
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
