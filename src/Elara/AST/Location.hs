{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Elara.AST.Location (
    AstNode (..),
    wrap,
    NodeLoc,
    Join,
    Widen,
    TaggedLocate (..),
    stripTag,
    stripTagAndLocation,
    tagLocated,
    retag,
    tag,
    widen,
    (<.>),
    getLocation,
    spanAs,
) where

import Elara.AST.Region
import Elara.Data.Pretty

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

newtype NodeLoc (n :: AstNode) loc = NodeLoc loc deriving (Eq, IsRegion, Ord, Pretty, Semigroup, Show)

nodeLoc :: Lens' (TaggedLocate n loc a) (NodeLoc n loc)
nodeLoc = lens (\(TaggedLocate l _) -> l) (\(TaggedLocate _ a) l -> TaggedLocate l a)

-- | Wrap turns any located element into a 'NodeLoc' for a given 'ASTNode'.
wrap :: forall n -> loc -> NodeLoc n loc
wrap n = NodeLoc @n

instance HasSourceRegion (NodeLoc n SourceRegion) where
    sourceRegion = lens (\(NodeLoc l) -> l) (\_ newLoc -> NodeLoc newLoc)

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
    widen (NodeLoc l) = NodeLoc l

instance Widen n n where widen = identity

instance Widen PatternNode DeclNode

instance Widen VarNode DeclNode

instance Widen ExprNode DeclNode

instance Widen VarNode TypeNode

instance Widen TypeNode DeclNode

instance Widen PatternNode ExprNode

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

instance HasLocation (NodeLoc ast loc) loc where getLocation (NodeLoc l) = l

-- | a version of 'Located' that is tagged with the specific node type it is located for
data TaggedLocate (n :: AstNode) loc a = TaggedLocate !(NodeLoc n loc) a
    deriving (Functor, Generic)

instance HasSourceRegion (TaggedLocate n SourceRegion a) where
    sourceRegion = nodeLoc % sourceRegion

instance Pretty a => Pretty (TaggedLocate n loc a) where
    pretty :: Pretty a => TaggedLocate n loc a -> Doc AnsiStyle
    pretty (TaggedLocate _ a) = pretty a

instance HasLocation (TaggedLocate n loc a) (NodeLoc n loc) where
    getLocation (TaggedLocate l _) = l

deriving instance (Show a, Show (NodeLoc n loc)) => Show (TaggedLocate n loc a)

deriving instance (Eq a, Eq (NodeLoc n loc)) => Eq (TaggedLocate n loc a)

deriving instance (Ord a, Ord (NodeLoc n loc)) => Ord (TaggedLocate n loc a)

instance LocatedElement (TaggedLocate n loc) where
    unlocated = lensVL $ \f (TaggedLocate l x) -> fmap (TaggedLocate l) (f x)

-- | Tag a value that 'HasLocation' with a specific located node type
tag :: forall (n :: AstNode) -> forall a loc. HasLocation a loc => a -> NodeLoc n loc
tag n x = NodeLoc @n (getLocation x)

-- | Convert a generic 'Located' value into a tagged 'TaggedLocate'
tagLocated :: forall n a. Located a -> TaggedLocate n SourceRegion a
tagLocated (Located region val) = TaggedLocate (NodeLoc region) val

spanAs :: forall n a loc. (HasLocation a loc, Monoid loc) => [a] -> NodeLoc n loc
spanAs xs = NodeLoc @n (mconcat (map getLocation xs))

retag ::
    forall oldTag loc a.
    forall newTag ->
    TaggedLocate oldTag loc a -> TaggedLocate newTag loc a
retag new (TaggedLocate (NodeLoc l) a) = TaggedLocate @new (NodeLoc l) a

stripTag :: forall n a. TaggedLocate n SourceRegion a -> Located a
stripTag (TaggedLocate (NodeLoc l) a) = Located l a

stripTagAndLocation :: forall n a. TaggedLocate n SourceRegion a -> a
stripTagAndLocation = view unlocated . stripTag
