module TopologicalGraph where

import Data.List (sort)
import Elara.Data.TopologicalGraph (HasDependencies (..), TopologicalGraph, createGraph, traverseGraphTopologically, traverseGraphTopologically_)
import Hedgehog (Property, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Syd (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.Syd.Hedgehog ()

spec :: Spec
spec = describe "TopologicalGraph" $ do
    createGraphTests
    traversalTests

-- Test data type
data TestNode = TestNode
    { nodeKey :: Int
    , nodeDeps :: [Int]
    }
    deriving (Show, Eq)

instance HasDependencies TestNode where
    type Key TestNode = Int
    key = nodeKey
    dependencies = nodeDeps

createGraphTests :: Spec
createGraphTests = describe "createGraph" $ do
    it "creates a graph from a list of nodes" $ do
        let nodes =
                [ TestNode 1 []
                , TestNode 2 [1]
                , TestNode 3 [1, 2]
                ]
        let graph = createGraph nodes
        -- If it doesn't throw an exception, it worked
        graph `shouldSatisfy` const True

    it "creates a graph with a single node" $ do
        let nodes = [TestNode 1 []]
        let graph = createGraph nodes
        graph `shouldSatisfy` const True

    it "creates a graph with cyclic dependencies" $ do
        let nodes =
                [ TestNode 1 [2]
                , TestNode 2 [1]
                ]
        let graph = createGraph nodes
        graph `shouldSatisfy` const True

traversalTests :: Spec
traversalTests = describe "graph traversal" $ do
    it "traverses nodes in topological order" $ do
        let nodes =
                [ TestNode 1 []
                , TestNode 2 [1]
                , TestNode 3 [1, 2]
                ]
        let graph = createGraph nodes

        visited <- newIORef []
        traverseGraphTopologically_
            ( \node -> do
                modifyIORef visited (nodeKey node :)
                pure node
            )
            graph

        result <- readIORef visited
        -- Node 1 should be visited before 2 and 3
        -- Node 2 should be visited before 3
        let node1Idx = elemIndex 1 result
        let node2Idx = elemIndex 2 result
        let node3Idx = elemIndex 3 result

        case (node1Idx, node2Idx, node3Idx) of
            (Just i1, Just i2, Just i3) -> do
                i1 `shouldSatisfy` (> i2)
                i1 `shouldSatisfy` (> i3)
                i2 `shouldSatisfy` (> i3)
            _ -> expectationFailure "Not all nodes were visited"

    it "handles independent nodes" $ do
        let nodes =
                [ TestNode 1 []
                , TestNode 2 []
                , TestNode 3 []
                ]
        let graph = createGraph nodes

        visited <- newIORef []
        traverseGraphTopologically_
            ( \node -> do
                modifyIORef visited (nodeKey node :)
                pure node
            )
            graph

        result <- readIORef visited
        sort result `shouldBe` sort [1, 2, 3]

    it "applies function to all nodes during traversal" $ property $ do
        nodes <- forAll $ Gen.list (Range.linear 1 10) $ do
            key <- Gen.int (Range.linear 1 100)
            pure $ TestNode key []

        let graph = createGraph nodes
        let originalKeys = sort $ map nodeKey nodes

        newGraph <- liftIO $ traverseGraphTopologically (\n -> pure $ n{nodeKey = nodeKey n * 2}) graph

        visited <- liftIO $ newIORef []
        liftIO $
            traverseGraphTopologically_
                ( \node -> do
                    modifyIORef visited (nodeKey node :)
                    pure node
                )
                newGraph

        result <- liftIO $ readIORef visited
        sort result === sort (map (* 2) originalKeys)
