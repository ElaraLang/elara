{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A generic, graph data structure that supports topological sorting.
module Elara.Data.TopologicalGraph where

import Data.Array
import Data.Containers.ListUtils (nubOrd)
import Data.Graph
import Elara.Data.Pretty
import Elara.Utils (uncurry3)
import Relude.Extra (firstF)
import Text.Show qualified as Show

class Ord (Key a) => HasDependencies a where
    type Key a
    keys :: a -> NonEmpty (Key a)
    keys = pure . key

    key :: a -> Key a
    key = head . keys

    dependencies :: a -> [Key a]

    {-# MINIMAL (keys | key), dependencies #-}

instance HasDependencies a => HasDependencies (a, b) where
    type Key (a, b) = Key a
    keys = keys . fst
    dependencies = dependencies . fst

data TopologicalGraph a = TopologicalGraph
    { _moduleGraph :: Graph
    , _nodeFromVertex :: Vertex -> (a, Key a, [Key a])
    , _vertexFromKey :: Key a -> Maybe Vertex
    }

makeLenses ''TopologicalGraph

mapGraph :: HasDependencies m => (a -> m) -> TopologicalGraph a -> TopologicalGraph m
mapGraph f = runIdentity . traverseGraph (pure . f)

-- | Traverse a graph. Due to the 'HasDependencies m' constraint we can't derive 'Traversable'
traverseGraph :: (HasDependencies m, Applicative f) => (a -> f m) -> TopologicalGraph a -> f (TopologicalGraph m)
traverseGraph = genericGraphTraverse vertices

traverseGraph_ :: Applicative f => (a -> f b) -> TopologicalGraph a -> f ()
traverseGraph_ = genericGraphTraverse_ vertices

-- | Traverse a graph in topological order
traverseGraphTopologically :: (HasDependencies m, Applicative f) => (a -> f m) -> TopologicalGraph a -> f (TopologicalGraph m)
traverseGraphTopologically = genericGraphTraverse topSort

-- | Traverse a graph in topological order, ignoring the results
traverseGraphTopologically_ :: Applicative f => (a -> f m) -> TopologicalGraph a -> f ()
traverseGraphTopologically_ = genericGraphTraverse_ topSort

-- | Traverse a graph in rev topological order
traverseGraphRevTopologically :: (HasDependencies m, Applicative f) => (a -> f m) -> TopologicalGraph a -> f (TopologicalGraph m)
traverseGraphRevTopologically = genericGraphTraverse reverseTopSort

-- | Traverse a graph in reverse topological order, ignoring the results
traverseGraphRevTopologically_ :: Applicative f => (a -> f m) -> TopologicalGraph a -> f ()
traverseGraphRevTopologically_ = genericGraphTraverse_ reverseTopSort

genericGraphTraverse :: (HasDependencies a, Applicative f) => (Graph -> [Vertex]) -> (m -> f a) -> TopologicalGraph m -> f (TopologicalGraph a)
genericGraphTraverse f f' g = do
    let sorted = f (g ^. moduleGraph)
    let sortedNodes = fmap ((^. _1) . (g ^. nodeFromVertex)) sorted
    createGraph <$> traverse f' sortedNodes

genericGraphTraverse_ :: Applicative f => (Graph -> [Vertex]) -> (m -> f a) -> TopologicalGraph m -> f ()
genericGraphTraverse_ f f' g = do
    let sorted = f (g ^. moduleGraph)
    let sortedNodes = fmap ((^. _1) . (g ^. nodeFromVertex)) sorted
    traverse_ f' sortedNodes

createGraph :: HasDependencies a => [a] -> TopologicalGraph a
createGraph = uncurry3 TopologicalGraph . graphFromEdges . mconcat . fmap (toList . createEdge)

createEdge ::
    forall a.
    HasDependencies a =>
    a ->
    NonEmpty (a, Key a, [Key a])
createEdge m = do
    let mns = keys m
    let mImports = dependencies m
    (m,,mImports) <$> mns

removeNode :: HasDependencies a => Key a -> TopologicalGraph a -> TopologicalGraph a
removeNode m g = createGraph (filter (\x -> key x /= m) (allEntries g))

addNode :: HasDependencies a => a -> TopologicalGraph a -> TopologicalGraph a
addNode m g = createGraph (m : allEntries g)

replaceNode :: HasDependencies a => a -> TopologicalGraph a -> TopologicalGraph a
replaceNode m g = createGraph (m : filter (\x -> key x /= key m) (allEntries g))

replaceNodeAt :: HasDependencies a => Key a -> a -> TopologicalGraph a -> TopologicalGraph a
replaceNodeAt m m' g = createGraph (m' : filter (\x -> key x /= m) (allEntries g))

allEntries :: TopologicalGraph m -> [m]
allEntries g = g ^.. moduleGraph % to vertices % each % to (g ^. nodeFromVertex) % _1

allEntriesTopologically :: Ord m => TopologicalGraph m -> [m]
allEntriesTopologically g = nubOrd (g ^.. moduleGraph % to topSort % each % to (g ^. nodeFromVertex) % _1)

-- | Get all entries in reverse topological order, but without duplicates
allEntriesRevTopologically :: Ord m => TopologicalGraph m -> [m]
allEntriesRevTopologically g = nubOrd (g ^.. moduleGraph % to reverseTopSort % each % to (g ^. nodeFromVertex) % _1)

dependenciesOf :: Key a -> TopologicalGraph a -> [Key a]
dependenciesOf m g = fromMaybe [] $ do
    let v = g ^. vertexFromKey
    let n = g ^. nodeFromVertex
    vertex <- v m
    let (_, _, deps) = n vertex
    pure deps

moduleFromName :: Key a -> TopologicalGraph a -> Maybe a
moduleFromName m g = do
    let v = g ^. vertexFromKey
    let n = g ^. nodeFromVertex
    vertex <- v m
    let (m', _, _) = n vertex
    pure m'

instance (Show a, Show (Key a)) => Show (TopologicalGraph a) where
    show g =
        let gArr = g ^. moduleGraph
            nodeFromVertex' = g ^. nodeFromVertex
            mnFromVertex = view (to nodeFromVertex' % _2)
            nodes = (mnFromVertex <<$>> gArr)
            assocs' = assocs nodes
         in Show.show (firstF mnFromVertex assocs')

instance (Pretty (Key a), Ord (Key a)) => Pretty (TopologicalGraph a) where
    pretty g =
        let gArr = g ^. moduleGraph % to vertices
            nodeFromVertex' = g ^. nodeFromVertex
            keyFromVertex = view (to nodeFromVertex' % _2)
            nodes = nubOrd (keyFromVertex <$> gArr)
         in pretty (map (\x -> (x, nubOrd $ dependenciesOf x g)) nodes)

instance Foldable TopologicalGraph where
    foldMap f g = foldMap f (allEntries g)
