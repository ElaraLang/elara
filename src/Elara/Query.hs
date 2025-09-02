{-# LANGUAGE TemplateHaskell #-}

module Elara.Query where

import Data.GADT.Compare.TH
import Data.GADT.Show.TH
import Data.Hashable (hash)
import Effectful
import Effectful.FileSystem (FileSystem)
import Elara.ReadFile (getInputFiles)
import Rock qualified

data Query (es :: [Effect]) a where
    -- | Query to get all the required input files to be passed to the compiler
    InputFiles :: Query '[FileSystem] (HashSet FilePath)

deriving instance Eq (Query es a)

deriving instance Show (Query es a)

deriveGEq ''Query
deriveGCompare ''Query
deriveGShow ''Query

instance Hashable (Query es a) where
    hashWithSalt salt = \case
        InputFiles -> h 0 ()
      where
        h :: Hashable b => Int -> b -> Int
        h tag payload =
            hash tag `hashWithSalt` payload `hashWithSalt` salt

rules :: Rock.Rules Query
rules key = do
    case key of
        InputFiles -> getInputFiles

-- runQuery :: (IOE :> es) => Query  -> Eff es a
runQuery :: Query es a -> Eff es a
runQuery query =
    Rock.runRock rules $ Rock.fetch query
