{-# LANGUAGE TemplateHaskell #-}

{- | We often need to perform a topological sort on modules
for things like type inference. This module provides a stable interface for generating and sorting a graph of modules
-}
module Elara.ModuleGraph where

import Control.Lens
import Data.Array
import Data.Graph (Graph, Vertex, graphFromEdges, reverseTopSort, topSort, vertices)
import Elara.AST.Module
import Elara.AST.Name (ModuleName)
import Elara.AST.Select (HasName (..), RUnlocate (..))
import Elara.Core.Module qualified as Core
import Elara.Data.Pretty
import Elara.Utils (uncurry3)
import Relude.Extra (firstF)
import Text.Show (Show (show))

class ModuleLike a where
    moduleName :: Lens' a ModuleName
    moduleDependencies :: a -> [ModuleName]

instance (RUnlocate ast) => ModuleLike (Module ast) where
    moduleName = name . rUnlocated' @ast
    moduleDependencies = toListOf (imports . each . importing . rUnlocated' @ast)

instance ModuleLike Core.Module where
    moduleName = Core.moduleName
    moduleDependencies = const []

data ModuleGraph m = ModuleGraph
    { _moduleGraph :: Graph
    , _nodeFromVertex :: Vertex -> (m, ModuleName, [ModuleName])
    , _vertexFromKey :: ModuleName -> Maybe Vertex
    }
    deriving (Functor)

makeLenses ''ModuleGraph

-- | Traverse a graph. Due to the 'ModuleLike m' constraint we can't derive 'Traversable'
traverseGraph :: (ModuleLike m, Applicative f) => (a -> f m) -> ModuleGraph a -> f (ModuleGraph m)
traverseGraph = genericGraphTraverse vertices

traverseGraph_ :: (Applicative f) => (a -> f b) -> ModuleGraph a -> f ()
traverseGraph_ = genericGraphTraverse_ vertices

-- | Traverse a graph in topological order
traverseGraphTopologically :: (ModuleLike m, Applicative f) => (a -> f m) -> ModuleGraph a -> f (ModuleGraph m)
traverseGraphTopologically = genericGraphTraverse topSort

-- | Traverse a graph in topological order, ignoring the results
traverseGraphTopologically_ :: (Applicative f) => (a -> f m) -> ModuleGraph a -> f ()
traverseGraphTopologically_ = genericGraphTraverse_ topSort

-- | Traverse a graph in rev topological order
traverseGraphRevTopologically :: (ModuleLike m, Applicative f) => (a -> f m) -> ModuleGraph a -> f (ModuleGraph m)
traverseGraphRevTopologically = genericGraphTraverse reverseTopSort

-- | Traverse a graph in reverse topological order, ignoring the results
traverseGraphRevTopologically_ :: (Applicative f) => (a -> f m) -> ModuleGraph a -> f ()
traverseGraphRevTopologically_ = genericGraphTraverse_ reverseTopSort

genericGraphTraverse :: (ModuleLike a, Applicative f) => (Graph -> [Vertex]) -> (m -> f a) -> ModuleGraph m -> f (ModuleGraph a)
genericGraphTraverse f f' g = do
    let sorted = f (g ^. moduleGraph)
    let sortedNodes = fmap ((^. _1) . (g ^. nodeFromVertex)) sorted
    createGraph <$> traverse f' sortedNodes

genericGraphTraverse_ :: (Applicative f) => (Graph -> [Vertex]) -> (m -> f a) -> ModuleGraph m -> f ()
genericGraphTraverse_ f f' g = do
    let sorted = f (g ^. moduleGraph)
    let sortedNodes = fmap ((^. _1) . (g ^. nodeFromVertex)) sorted
    traverse_ f' sortedNodes

createEdge ::
    forall a.
    (ModuleLike a) =>
    a ->
    (a, ModuleName, [ModuleName])
createEdge m = do
    let mn = m ^. moduleName
    let mImports = moduleDependencies m
    (m, mn, mImports)

createGraph :: (ModuleLike a) => [a] -> ModuleGraph a
createGraph = uncurry3 ModuleGraph . graphFromEdges . fmap createEdge

allEntries :: ModuleGraph m -> [m]
allEntries g = g ^.. moduleGraph . to vertices . each . to (g ^. nodeFromVertex) . _1

dependenciesOf :: ModuleName -> ModuleGraph ast -> [ModuleName]
dependenciesOf m g = fromMaybe [] $ do
    let v = g ^. vertexFromKey
    let n = g ^. nodeFromVertex
    vertex <- v m
    let (_, _, deps) = n vertex
    pure deps

moduleFromName :: ModuleName -> ModuleGraph m -> Maybe m
moduleFromName m g = do
    let v = g ^. vertexFromKey
    let n = g ^. nodeFromVertex
    vertex <- v m
    let (m', _, _) = n vertex
    pure m'

instance (Show m) => Show (ModuleGraph m) where
    show g =
        let gArr = g ^. moduleGraph
            nodeFromVertex' = g ^. nodeFromVertex
            mnFromVertex = view (to nodeFromVertex' . _2)
            nodes = (mnFromVertex <<$>> gArr)
            assocs' = assocs nodes
         in Text.Show.show (firstF mnFromVertex assocs')

instance Pretty (ModuleGraph m) where
    pretty g =
        let gArr = g ^. moduleGraph
            nodeFromVertex' = g ^. nodeFromVertex
            mnFromVertex = view (to nodeFromVertex' . _2)
            nodes = (mnFromVertex <<$>> gArr)
            assocs' = assocs nodes
         in pretty (firstF mnFromVertex assocs')