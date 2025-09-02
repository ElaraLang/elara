{-# LANGUAGE TemplateHaskell #-}

module Elara.Query where

import Data.GADT.Compare.TH
import Data.GADT.Show.TH
import Data.Hashable (hash)
import Effectful
import Effectful.FileSystem (FileSystem)

import Effectful.Error.Static (Error)
import Elara.Lexer.Token
import Elara.Lexer.Utils (LexerError)
import Rock (Rock)

data Query (es :: [Effect]) a where
    -- | Query to get all the required input files to be passed to the compiler
    InputFiles :: Query '[FileSystem] (HashSet FilePath)
    GetFileContents :: FilePath -> Query '[FileSystem] Text
    LexedFile :: FilePath -> Query '[FileSystem, Rock Query, Error LexerError] [Lexeme]

deriving instance Eq (Query es a)

deriving instance Show (Query es a)

deriveGEq ''Query
deriveGCompare ''Query
deriveGShow ''Query

instance Hashable (Query es a) where
    hashWithSalt salt = \case
        InputFiles -> h 0 ()
        GetFileContents fp -> h 1 fp
        LexedFile fp -> h 2 fp
      where
        h :: Hashable b => Int -> b -> Int
        h tag payload =
            hash tag `hashWithSalt` payload `hashWithSalt` salt
