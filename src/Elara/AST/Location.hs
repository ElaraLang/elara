{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Elara.AST.Location where

import Elara.AST.Region
import Elara.Data.Pretty
import GHC.TypeError

{- | The different kinds of nodes that should have incompatible locations
to prevent accidentally using the wrong location for a node.
the actual definitions of what is in each category is a bit vague
-}
type data AstNode
    = -- | a location of the _whole_ module
      ModuleNode
    | -- | a location of an entire declaration
      DeclNode
    | -- | a location of an expression
      ExprNode
    | -- | a location of a pattern
      PatternNode
    | -- | a location of a type
      TypeNode
    | -- | a location of a variable
      VarNode

data family NodeLoc (n :: AstNode) loc

newtype instance NodeLoc ModuleNode loc = ModuleLoc loc deriving (Show, Eq, Ord, Semigroup, IsRegion, Pretty)

newtype instance NodeLoc DeclNode loc = DeclLoc loc deriving (Show, Eq, Ord, Semigroup, IsRegion, Pretty)

newtype instance NodeLoc ExprNode loc = ExprLoc loc deriving (Show, Eq, Ord, Semigroup, IsRegion, Pretty)

newtype instance NodeLoc VarNode loc = VarLoc loc deriving (Show, Eq, Ord, Semigroup, IsRegion, Pretty)

newtype instance NodeLoc TypeNode loc = TypeLoc loc deriving (Show, Eq, Ord, Semigroup, IsRegion, Pretty)

newtype instance NodeLoc PatternNode loc = PatLoc loc deriving (Show, Eq, Ord, Semigroup, IsRegion, Pretty)

class UnwrapNodeLoc (n :: AstNode) loc a where
    unwrapLoc :: NodeLoc n loc -> a

instance UnwrapNodeLoc ModuleNode loc loc where unwrapLoc (ModuleLoc l) = l

instance UnwrapNodeLoc DeclNode loc loc where unwrapLoc (DeclLoc l) = l

instance UnwrapNodeLoc ExprNode loc loc where unwrapLoc (ExprLoc l) = l

instance UnwrapNodeLoc VarNode loc loc where unwrapLoc (VarLoc l) = l

instance UnwrapNodeLoc TypeNode loc loc where unwrapLoc (TypeLoc l) = l

instance UnwrapNodeLoc PatternNode loc loc where unwrapLoc (PatLoc l) = l

{- | the "join" of two nodes, i.e. the smallest node that subsumes both of them.
note that this is not the same as a "container" relationship. for example, an 'ExprNode' can contain a 'PatternNode', since a pattern can appear inside an expression,
but the join of 'ExprNode' and 'PatternNode' is 'ExprNode', since 'ExprNode' is the only node that could contain both at once.
@a@ should be the smaller node and @b@ the larger node.
by default, this relation is symmetric, so should be overridden for asymmetric cases.
-}
type family Join (a :: AstNode) (b :: AstNode) :: AstNode where
    -- Identity
    Join n n = n
    -- Join PatternNode ExprNode = ExprNode -- an expression can contain a pattern
    Join ExprNode PatternNode =
        ExprNode
    Join VarNode ExprNode = DeclNode
    Join VarNode TypeNode = TypeNode -- a (type) variable can appear in a type
    Join DeclNode n = DeclNode
    Join n DeclNode = DeclNode
    Join ModuleNode _ = ModuleNode -- everything is contained in the module
    Join a b = Join b a

-- | A node @a@ subsumes a node @b@ if the join of @a@ and @b@ is @a@
type Subsumes narrower wider = (Join narrower wider ~ wider)

-- | Widen a node to a wider node at the data level
class Subsumes a b => Widen (a :: AstNode) (b :: AstNode) where
    widen :: NodeLoc a loc -> NodeLoc b loc

instance Widen n n where widen = identity

instance Widen PatternNode DeclNode where widen (PatLoc l) = DeclLoc l

instance Widen VarNode DeclNode where widen (VarLoc l) = DeclLoc l

instance Widen ExprNode DeclNode where widen (ExprLoc l) = DeclLoc l

instance Widen VarNode TypeNode where widen (VarLoc l) = TypeLoc l

instance Widen TypeNode DeclNode where widen (TypeLoc l) = DeclLoc l
instance Widen PatternNode ExprNode where widen (PatLoc l) = ExprLoc l

class LocSemigroup a b loc where
    (<.>) :: NodeLoc a loc -> NodeLoc b loc -> NodeLoc (Join a b) loc

infixr 6 <.>

instance
    ( IsRegion loc
    , Widen a (Join a b)
    , Widen b (Join a b)
    , Semigroup (NodeLoc (Join a b) loc)
    ) =>
    LocSemigroup a b loc
    where
    l1 <.> l2 =
        let raw1 = widen @a @(Join a b) l1
            raw2 = widen @b @(Join a b) l2
         in (raw1 <> raw2)

class HasLocation a loc | a -> loc where
    getLocation :: a -> loc

instance HasLocation (Located n) SourceRegion where getLocation (Located sr _) = sr

instance HasLocation (NodeLoc ModuleNode loc) loc where getLocation (ModuleLoc l) = l

instance HasLocation (NodeLoc DeclNode loc) loc where getLocation (DeclLoc l) = l

instance HasLocation (NodeLoc ExprNode loc) loc where getLocation (ExprLoc l) = l

instance HasLocation (NodeLoc PatternNode loc) loc where getLocation (PatLoc l) = l

instance HasLocation (NodeLoc VarNode loc) loc where getLocation (VarLoc l) = l

instance HasLocation (NodeLoc TypeNode loc) loc where getLocation (TypeLoc l) = l

-- | class for wrapping a raw location in a node-specific location wrapper
class WrapNode (n :: AstNode) where
    wrap :: loc -> NodeLoc n loc

instance WrapNode ModuleNode where wrap = ModuleLoc

instance WrapNode DeclNode where wrap = DeclLoc

instance WrapNode ExprNode where wrap = ExprLoc

instance WrapNode PatternNode where wrap = PatLoc

instance WrapNode VarNode where wrap = VarLoc

instance WrapNode TypeNode where wrap = TypeLoc

-- | a version of 'Located' that is tagged with the specific node type it is located for
data TaggedLocate (n :: AstNode) loc a = TaggedLocate !(NodeLoc n loc) a
    deriving (Generic, Functor)

instance Pretty a => Pretty (TaggedLocate n loc a) where
    pretty (TaggedLocate _ a) = pretty a

instance HasLocation (TaggedLocate n loc a) (NodeLoc n loc) where
    getLocation (TaggedLocate l _) = l

deriving instance (Show a, Show (NodeLoc n loc)) => Show (TaggedLocate n loc a)

deriving instance (Eq a, Eq (NodeLoc n loc)) => Eq (TaggedLocate n loc a)

deriving instance (Ord a, Ord (NodeLoc n loc)) => Ord (TaggedLocate n loc a)

instance LocatedElement (TaggedLocate n loc) where
    unlocated = lensVL $ \f (TaggedLocate l x) -> fmap (TaggedLocate l) (f x)

-- | Tag a value that 'HasLocation' with a specific located node type
tag :: forall (n :: AstNode) -> forall a loc. (HasLocation a loc, WrapNode n) => a -> NodeLoc n loc
tag n x = wrap @n (getLocation x)

-- | Convert a generic 'Located' value into a tagged 'TaggedLocate'
tagLocated :: forall n a. WrapNode n => Located a -> TaggedLocate n SourceRegion a
tagLocated (Located region val) = TaggedLocate (wrap @n region) val

spanAs :: forall n a loc. (HasLocation a loc, WrapNode n, Monoid loc) => [a] -> NodeLoc n loc
spanAs xs = wrap @n (mconcat (map getLocation xs))

retag ::
    forall newTag oldTag loc a.
    (UnwrapNodeLoc oldTag loc loc, WrapNode newTag) =>
    TaggedLocate oldTag loc a -> TaggedLocate newTag loc a
retag (TaggedLocate l a) = TaggedLocate (wrap @newTag (unwrapLoc l)) a

stripTag :: forall n loc a. UnwrapNodeLoc n loc SourceRegion => TaggedLocate n loc a -> Located a
stripTag (TaggedLocate l a) = Located (unwrapLoc l) a
