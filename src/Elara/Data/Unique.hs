{-# LANGUAGE TemplateHaskell #-}

module Elara.Data.Unique where

import Control.Lens.TH (makeLenses)
import Data.Data (Data)
import Elara.Data.Pretty
import GHC.IO (unsafePerformIO)
import Polysemy (Member, Sem, embed, makeSem, reinterpret)
import Polysemy.Embed (Embed)
import Polysemy.State (State, get, put)
import Text.Show (Show (show))

data Unique a = Unique
    { _uniqueVal :: !a
    , _uniqueId :: !Integer
    }
    deriving (Show, Functor, Data)

unsafeMkUnique :: a -> Integer -> Unique a
unsafeMkUnique = Unique

-- | A @Unique@ where the value is not important.
newtype UniqueId = UniqueId (Unique ()) deriving (Eq, Ord, Data)

instance Show UniqueId where
    show (UniqueId u) = Text.Show.show (_uniqueId u)

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
    NewUniqueNum :: UniqueGen m Integer

uniqueGenToState :: Sem (UniqueGen ': r) a -> Sem (State UniqueSupply : r) a
uniqueGenToState = reinterpret $ \case
    NewUniqueNum -> do
        (UniqueSupply us) <- get
        case us of
            [] -> error "Ran out of unique IDs! Should be impossible."
            (i : is) -> do
                put (UniqueSupply is)
                pure i

uniqueGenToIO :: Sem (UniqueGen ': r) a -> Sem (Embed IO : r) a
uniqueGenToIO = reinterpret $ \case
    NewUniqueNum -> do
        us <- embed (readIORef globalUniqueSupply)
        case _uniqueSupplyUniques us of
            [] -> error "Ran out of unique IDs! Should be impossible."
            (i : is) -> do
                embed (writeIORef globalUniqueSupply (UniqueSupply is))
                pure i

makeSem ''UniqueGen
makeLenses ''UniqueSupply
makeLenses ''Unique

makeUniqueId :: Member UniqueGen r => Sem r UniqueId
makeUniqueId = UniqueId <$> makeUnique ()

makeUnique :: Member UniqueGen r => a -> Sem r (Unique a)
makeUnique a = Unique a <$> newUniqueNum

instance Pretty a => Pretty (Unique a) where
    pretty (Unique a i) = pretty a <> "_" <> pretty i

instance Pretty UniqueId where
    pretty (UniqueId (Unique _ i)) = pretty i