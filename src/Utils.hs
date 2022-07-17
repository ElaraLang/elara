module Utils where

import Data.Map qualified as M

associateWithKey :: Ord k => (a -> k) -> [a] -> M.Map k a
associateWithKey f = M.fromList . map (\x -> (f x, x))