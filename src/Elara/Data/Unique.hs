{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Elara.Data.Unique where

import Data.Aeson (ToJSON)
import Data.Data (Data)
import Elara.Data.Pretty
import GHC.IO (unsafePerformIO)
import Text.Show (Show (show))

data Unique a = Unique !a !Int
    deriving (Show, Functor, Data, Generic, Traversable, Foldable)

pattern Unique' :: a -> Unique a
pattern Unique' a <- Unique a _

{-# COMPLETE Unique' #-}

uniqueId :: Lens' (Unique a) Int
uniqueId = lens (\(Unique _ i) -> i) (\(Unique a _) i -> Unique a i)

uniqueVal :: Lens' (Unique a) a
uniqueVal = lens (\(Unique a _) -> a) (\(Unique _ i) a -> Unique a i)

unsafeMkUnique :: a -> Int -> Unique a
unsafeMkUnique = Unique

-- | A @Unique@ where the value is not important.
newtype UniqueId = UniqueId (Unique ()) deriving (Eq, Ord, Data, Generic)

instance ToJSON c => ToJSON (Unique c)

instance ToJSON UniqueId

instance Show UniqueId where
    show (UniqueId (Unique _ uniqueId)) = Text.Show.show uniqueId

instance Eq a => Eq (Unique a) where
    (Unique a i) == (Unique b j) = i == j && a == b

instance Ord a => Ord (Unique a) where
    compare (Unique a i) (Unique b j) = case compare i j of
        EQ -> compare a b
        other -> other

newtype UniqueSupply = UniqueSupply
    { _uniqueSupplyUniques :: [Int]
    }

freshUniqueSupply :: UniqueSupply
freshUniqueSupply = UniqueSupply [0 ..]

globalUniqueSupply :: IORef UniqueSupply
globalUniqueSupply = unsafePerformIO (newIORef freshUniqueSupply)
{-# NOINLINE globalUniqueSupply #-}

resetGlobalUniqueSupply :: IO ()
resetGlobalUniqueSupply = writeIORef globalUniqueSupply freshUniqueSupply

makeLenses ''UniqueSupply
makeLenses ''Unique

getUniqueId :: Unique a -> UniqueId
getUniqueId = UniqueId . void

uniqueToText :: (a -> Text) -> Unique a -> Text
uniqueToText f (Unique a i) = f a <> Prelude.show i

instance Pretty a => Pretty (Unique a) where
    pretty (Unique a i) = pretty a <> "_" <> pretty i

instance Pretty UniqueId where
    pretty (UniqueId (Unique _ i)) = pretty i

instance Hashable b => Hashable (Unique b)

instance Hashable UniqueId
