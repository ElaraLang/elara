module Elara.Core.Generic where

import Data.Data (Data)
import Relude.Extra (bimapF)

data Bind b expr
    = Recursive [(b, expr b)]
    | NonRecursive (b, expr b)
    deriving (Show, Eq, Data, Generic)

binds :: Bind b expr -> [(b, expr b)]
binds = \case
    Recursive bs -> bs
    NonRecursive b -> [b]

binders :: Bind b expr -> [b]
binders = map fst . binds

mapBind :: (b -> b') -> (expr b -> expr' b') -> Bind b expr -> Bind b' expr'
mapBind f g = \case
    Recursive bs -> Recursive (bimapF f g bs)
    NonRecursive (b, e) -> NonRecursive (f b, g e)

traverseBind :: Applicative f => (b -> f b') -> (expr b -> f (expr' b')) -> Bind b expr -> f (Bind b' expr')
traverseBind f g = \case
    Recursive bs -> Recursive <$> traverse (\(b, e) -> (,) <$> f b <*> g e) bs
    NonRecursive (b, e) -> NonRecursive <$> ((,) <$> f b <*> g e)
