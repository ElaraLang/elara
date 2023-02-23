module Elara.Parse.Module where

import Elara.AST.Module (Exposing (..), Exposition (ExposedOp, ExposedValue), Import (..), Import' (..), Module (..), Module' (..))
import Elara.AST.Name
import Elara.AST.Region
import Elara.AST.Select
import Elara.Parse.Declaration (declaration)
import Elara.Parse.Names (opName, varName)
import Elara.Parse.Names qualified as Parse (moduleName)
import Elara.Parse.Primitives
import HeadedMegaparsec (endHead)
import Text.Megaparsec (sepEndBy)

module' :: HParser (Module Frontend)
module' = fmapLocated Module $ do
    mHeader <- optional header
    thisFile <- sourceFile . getLocation <$> located pass
    let _name = maybe (Located (SourceRegion thisFile 0 4) (ModuleName ("Main" :| []))) fst mHeader
    skipNewlines
    imports <- import' `sepEndBy` skipNewlines
    declarations <- declaration _name `sepEndBy` skipNewlines

    pure $
        Module'
            { _module'Name = _name
            , _module'Exposing = maybe ExposingAll snd mHeader
            , _module'Imports = imports
            , _module'Declarations = declarations
            }

header :: HParser (Located ModuleName, Exposing Frontend)
header = do
    -- module Name exposing (..)
    symbol "module"
    endHead
    moduleName' <- located $ lexeme Parse.moduleName
    exposing' <- exposing
    pure (moduleName', exposing')

exposing :: HParser (Exposing Frontend)
exposing =
    fromMaybe ExposingAll
        <$> optional
            ( do
                symbol "exposing"
                es <- lexeme (oneOrCommaSeparatedInParens exposition)
                pure $ ExposingSome es
            )

exposition :: HParser (Exposition Frontend)
exposition = exposedValue <|> exposedOp
  where
    exposedValue, exposedOp :: HParser (Exposition Frontend)
    exposedValue = ExposedValue <$> located varName
    exposedOp = ExposedOp <$> located (inParens opName)

import' :: HParser (Import Frontend)
import' = fmapLocated Import $ do
    symbol "import"
    endHead
    moduleName' <- located $ lexeme Parse.moduleName
    isQualified <- isJust <$> optional (symbol "qualified")
    as <- optional . located $ do
        symbol "as"
        lexeme Parse.moduleName
    Import' moduleName' as isQualified <$> exposing