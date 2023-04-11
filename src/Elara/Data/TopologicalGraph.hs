{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A generic, graph data structure that supports topological sorting.
module Elara.Data.TopologicalGraph where

import Control.Lens
import Data.Array
import Data.Graph
import Elara.Data.Pretty
import Elara.Utils (uncurry3)
import Relude.Extra (firstF)
import Text.Show qualified as Show

class (Ord (Key a)) => HasDependencies a where
    type Key a
    key :: a -> Key a
    dependencies :: a -> [Key a]

data TopologicalGraph a = TopologicalGraph
    { _moduleGraph :: Graph
    , _nodeFromVertex :: Vertex -> (a, Key a, [Key a])
    , _vertexFromKey :: Key a -> Maybe Vertex
    }

makeLenses ''TopologicalGraph

-- | Traverse a graph. Due to the 'HasDependencies m' constraint we can't derive 'Traversable'
traverseGraph :: (HasDependencies m, Applicative f) => (a -> f m) -> TopologicalGraph a -> f (TopologicalGraph m)
traverseGraph = genericGraphTraverse vertices

traverseGraph_ :: (Applicative f) => (a -> f b) -> TopologicalGraph a -> f ()
traverseGraph_ = genericGraphTraverse_ vertices

-- | Traverse a graph in topological order
traverseGraphTopologically :: (HasDependencies m, Applicative f) => (a -> f m) -> TopologicalGraph a -> f (TopologicalGraph m)
traverseGraphTopologically = genericGraphTraverse topSort

-- | Traverse a graph in topological order, ignoring the results
traverseGraphTopologically_ :: (Applicative f) => (a -> f m) -> TopologicalGraph a -> f ()
traverseGraphTopologically_ = genericGraphTraverse_ topSort

-- | Traverse a graph in rev topological order
traverseGraphRevTopologically :: (HasDependencies m, Applicative f) => (a -> f m) -> TopologicalGraph a -> f (TopologicalGraph m)
traverseGraphRevTopologically = genericGraphTraverse reverseTopSort

-- | Traverse a graph in reverse topological order, ignoring the results
traverseGraphRevTopologically_ :: (Applicative f) => (a -> f m) -> TopologicalGraph a -> f ()
traverseGraphRevTopologically_ = genericGraphTraverse_ reverseTopSort

genericGraphTraverse :: (HasDependencies a, Applicative f) => (Graph -> [Vertex]) -> (m -> f a) -> TopologicalGraph m -> f (TopologicalGraph a)
genericGraphTraverse f f' g = do
    let sorted = f (g ^. moduleGraph)
    let sortedNodes = fmap ((^. _1) . (g ^. nodeFromVertex)) sorted
    createGraph <$> traverse f' sortedNodes

genericGraphTraverse_ :: (Applicative f) => (Graph -> [Vertex]) -> (m -> f a) -> TopologicalGraph m -> f ()
genericGraphTraverse_ f f' g = do
    let sorted = f (g ^. moduleGraph)
    let sortedNodes = fmap ((^. _1) . (g ^. nodeFromVertex)) sorted
    traverse_ f' sortedNodes

createGraph :: (HasDependencies a) => [a] -> TopologicalGraph a
createGraph = uncurry3 TopologicalGraph . graphFromEdges . fmap createEdge

createEdge ::
    forall a.
    (HasDependencies a) =>
    a ->
    (a, Key a, [Key a])
createEdge m = do
    let mn = key m
    let mImports = dependencies m
    (m, mn, mImports)

allEntries :: TopologicalGraph m -> [m]
allEntries g = g ^.. moduleGraph . to vertices . each . to (g ^. nodeFromVertex) . _1

allEntriesTopologically :: TopologicalGraph m -> [m]
allEntriesTopologically g = g ^.. moduleGraph . to topSort . each . to (g ^. nodeFromVertex) . _1

allEntriesRevTopologically :: TopologicalGraph m -> [m]
allEntriesRevTopologically g = g ^.. moduleGraph . to reverseTopSort . each . to (g ^. nodeFromVertex) . _1

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
            mnFromVertex = view (to nodeFromVertex' . _2)
            nodes = (mnFromVertex <<$>> gArr)
            assocs' = assocs nodes
         in Show.show (firstF mnFromVertex assocs')

instance (Pretty a, Pretty (Key a)) => Pretty (TopologicalGraph a) where
    pretty g =
        let gArr = g ^. moduleGraph
            nodeFromVertex' = g ^. nodeFromVertex
            mnFromVertex = view (to nodeFromVertex' . _2)
            nodes = (mnFromVertex <<$>> gArr)
            assocs' = assocs nodes
         in pretty (firstF mnFromVertex assocs')