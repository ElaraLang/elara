-- | A list that contains at least two elements, used for tuples
module Elara.Data.AtLeast2List where

data AtLeast2List a = AtLeast2List a a [a]
    deriving (Eq, Show, Ord)

toList :: AtLeast2List a -> [a]
toList (AtLeast2List x y xs) = x : y : xs

toNonEmpty :: AtLeast2List a -> NonEmpty a
toNonEmpty (AtLeast2List x y xs) = x :| (y : xs)

fromList :: [a] -> Maybe (AtLeast2List a)
fromList (x : y : xs) = Just $ AtLeast2List x y xs
fromList _ = Nothing

fromNonEmpty :: NonEmpty a -> Maybe (AtLeast2List a)
fromNonEmpty (x :| (y : xs)) = Just $ AtLeast2List x y xs
fromNonEmpty _ = Nothing

fromHeadAndTail :: a -> NonEmpty a -> AtLeast2List a
fromHeadAndTail x (y :| ys) = AtLeast2List x y ys

instance Functor AtLeast2List where
    fmap f (AtLeast2List x y xs) = AtLeast2List (f x) (f y) (fmap f xs)

instance Foldable AtLeast2List where
    foldMap f (AtLeast2List x y xs) = f x <> f y <> foldMap f xs

instance Traversable AtLeast2List where
    traverse f (AtLeast2List x y xs) = AtLeast2List <$> f x <*> f y <*> traverse f xs

instance Semigroup (AtLeast2List a) where
    (AtLeast2List x1 y1 xs1) <> (AtLeast2List x2 y2 xs2) = AtLeast2List x1 y1 (xs1 <> (x2 : y2 : xs2))
