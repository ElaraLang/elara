module Utils where

import Control.Monad.Except (MonadError (throwError))
import Data.Map qualified as M
import Data.Multimap qualified as Mu
import Relude.Extra (fmapToFst)

associateWithKey :: Ord k => (a -> k) -> [a] -> M.Map k a
associateWithKey f = M.fromList . fmapToFst f

associateManyWithKey :: Ord k => (a -> k) -> [a] -> Mu.ListMultimap k a
associateManyWithKey f = Mu.fromList . fmapToFst f

mergeManyWithKey :: Ord k => (a -> k) -> (a -> a -> a) -> [a] -> M.Map k a
mergeManyWithKey f mergeFunction values = do
  let pairs = fmapToFst f values
  M.fromListWithKey (\_ e1 e2 -> mergeFunction e1 e2) pairs

uniquelyAssociateWithKey :: (Ord k) => (a -> k) -> [a] -> Either [a] (M.Map k a)
uniquelyAssociateWithKey f xs = do
  let associate = associateWithKey f xs
   in if length (M.keys associate) == length xs
        then pure associate
        else
          let duplicates =
                -- find the elements that appear more than once in the list
                filter (\x -> length (filter (\y -> f x == f y) xs) > 1) xs
           in throwError duplicates