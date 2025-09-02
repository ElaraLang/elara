module Elara.Rules where

import Effectful
import Effectful.FileSystem.IO.ByteString as Eff
import Elara.Lexer.Reader (getLexedFile)
import Elara.Query
import Elara.ReadFile (getInputFiles)
import Rock qualified
import Prelude hiding (withFile)

rules :: Rock.Rules Query
rules key = do
    case key of
        InputFiles -> getInputFiles
        GetFileContents fp -> do
            contents <- Eff.readFile fp
            pure $ decodeUtf8 contents
        LexedFile fp -> getLexedFile fp

runQuery :: Query es a -> Eff es a
runQuery query =
    Rock.runRock rules $ Rock.fetch query
