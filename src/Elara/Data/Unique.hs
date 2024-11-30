{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Elara.Data.Unique where

import Data.Aeson (ToJSON)
import Data.Data (Data)
import Elara.Data.Pretty
import GHC.IO (unsafePerformIO)
import Polysemy (Member, Sem, interpret, makeSem, reinterpret)
import Polysemy.AtomicState (AtomicState, atomicGet, atomicPut, atomicStateToIO)
import Polysemy.Embed (Embed)
import Polysemy.State (State, evalState, get, put)
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

instance Eq (Unique a) where
    (==) = (==) `on` view uniqueId

instance Ord (Unique a) where
    compare = compare `on` view uniqueId

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

data UniqueGen m a where
    NewUniqueNum :: UniqueGen m Int

-- runFreshUniqueSupply :: Sem (UniqueGen ': r) a -> Sem r a
-- runFreshUniqueSupply = evalState freshUniqueSupply . uniqueGenToState

uniqueGenToState :: Sem (UniqueGen ': r) a -> Sem (AtomicState UniqueSupply : r) a
uniqueGenToState = reinterpret $ \case
    NewUniqueNum -> do
        (UniqueSupply us) <- atomicGet
        case us of
            [] -> error "Ran out of unique IDs! Should be impossible."
            (i : is) -> do
                atomicPut (UniqueSupply is)
                pure i

uniqueGenToIO :: Member (Embed IO) r => Sem (UniqueGen ': r) a -> Sem r a
uniqueGenToIO = fmap snd . atomicStateToIO freshUniqueSupply . uniqueGenToState

makeSem ''UniqueGen
makeLenses ''UniqueSupply
makeLenses ''Unique

getUniqueId :: Unique a -> UniqueId
getUniqueId = UniqueId . void

makeUniqueId :: Member UniqueGen r => Sem r UniqueId
makeUniqueId = UniqueId <$> makeUnique ()

makeUnique :: Member UniqueGen r => a -> Sem r (Unique a)
makeUnique a = Unique a <$> newUniqueNum

uniqueToText :: (a -> Text) -> Unique a -> Text
uniqueToText f (Unique a i) = f a <> Prelude.show i

instance Pretty a => Pretty (Unique a) where
    pretty (Unique a i) = pretty a <> "_" <> pretty i

instance Pretty UniqueId where
    pretty (UniqueId (Unique _ i)) = pretty i

instance Hashable b => Hashable (Unique b)

instance Hashable UniqueId
