module Data.List.Extra where

isRearrangementOf :: (Eq a, Foldable t) => t a -> t a -> Bool
isRearrangementOf xs ys = isRearrangementOf' (toList xs) (toList ys)

isRearrangementOf' :: Eq a => [a] -> [a] -> Bool
isRearrangementOf' x y = all (`elem` y) x && all (`elem` x) y