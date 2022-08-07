module Utils where

import Control.Monad.Except (MonadError (throwError))
import Data.Map qualified as M
import Relude.Extra (fmapToFst)

associateWithKey :: Ord k => (a -> k) -> [a] -> M.Map k a
associateWithKey f = M.fromList . fmapToFst f

uniquelyAssociateWithKey :: (Ord k) => (a -> k) -> [a] -> Either [a] (M.Map k a)
uniquelyAssociateWithKey f xs = do
  let associate = associateWithKey f xs
   in if length (M.keys associate) == length xs
        then return associate
        else
          let duplicates =
                -- find the elements that appear more than once in the list
                filter (\x -> length (filter (\y -> f x == f y) xs) > 1) xs
           in throwError duplicates