{-# LANGUAGE TemplateHaskell #-}

module Elara.Data.Unique where

import Control.Lens.TH (makeLenses)
import GHC.IO (unsafePerformIO)
import Polysemy (Member, Sem, embed, interpret, makeSem)
import Polysemy.Embed (Embed)
import Polysemy.State (State, get, put)

data Unique a = Unique
    { _uniqueVal :: !a
    , _uniqueId :: !Integer
    }
    deriving (Show)

instance Eq (Unique a) where
    (==) = (==) `on` _uniqueId

instance Ord (Unique a) where
    compare = compare `on` _uniqueId

newtype UniqueSupply = UniqueSupply
    { _uniqueSupplyUniques :: [Integer]
    }

freshUniqueSupply :: UniqueSupply
freshUniqueSupply = UniqueSupply [0 ..]

globalUniqueSupply :: IORef UniqueSupply
globalUniqueSupply = unsafePerformIO (newIORef freshUniqueSupply)
{-# NOINLINE globalUniqueSupply #-}

data UniqueGen m a where
    MakeUnique :: a -> UniqueGen m (Unique a)

uniqueGenToState :: Member (State UniqueSupply) r => Sem (UniqueGen ': r) a -> Sem r a
uniqueGenToState = interpret $ \case
    MakeUnique a -> do
        (UniqueSupply us) <- get
        case us of
            [] -> error "Ran out of unique IDs! Should be impossible."
            (i : is) -> do
                put (UniqueSupply is)
                pure (Unique a i)

uniqueGenToIO :: Member (Embed IO) r => Sem (UniqueGen ': r) a -> Sem r a
uniqueGenToIO = interpret $ \case
    MakeUnique a -> do
        us <- embed (readIORef globalUniqueSupply)
        case _uniqueSupplyUniques us of
            [] -> error "Ran out of unique IDs! Should be impossible."
            (i : is) -> do
                embed (writeIORef globalUniqueSupply (UniqueSupply is))
                pure (Unique a i)

makeSem ''UniqueGen
makeLenses ''UniqueSupply
