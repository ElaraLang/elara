module Elara.Parse.Module where

import Elara.AST.Module (Exposing (..), Exposition (ExposedOp, ExposedValue), Import (..), Import' (..), Module (..), Module' (..))
import Elara.AST.Name
import Elara.AST.Region
import Elara.AST.Select
import Elara.Lexer.Token (Token (..))
import Elara.Parse.Declaration (declaration)
import Elara.Parse.Names (maybeQualified, opName, varName)
import Elara.Parse.Names qualified as Parse (moduleName)
import Elara.Parse.Primitives
import HeadedMegaparsec (endHead)
import Text.Megaparsec (MonadParsec (..), PosState (pstateSourcePos), SourcePos (sourceName), State (statePosState), sepEndBy)

module' :: HParser (Module Frontend)
module' = fmapLocated Module $ do
    mHeader <- optional (header <* optional (token' TokenSemicolon))
    endHead
    thisFile <- sourceName . pstateSourcePos . statePosState <$> fromParsec getParserState
    let _name = maybe (Located (GeneratedRegion thisFile) (ModuleName ("Main" :| []))) fst mHeader
    imports <- sepEndBy import' (token' TokenSemicolon)
    _ <- optional (token' TokenSemicolon)
    declarations <- sepEndBy (declaration _name) (token' TokenSemicolon)

    pure $
        Module'
            { _module'Name = _name
            , _module'Exposing = maybe ExposingAll snd mHeader
            , _module'Imports = imports
            , _module'Declarations = declarations
            }

-- | module Name exposing (..)
header :: HParser (Located ModuleName, Exposing Frontend)
header = do
    token' TokenModule
    endHead
    moduleName' <- located Parse.moduleName
    exposing' <- exposing
    pure (moduleName', exposing')

exposing :: HParser (Exposing Frontend)
exposing =
    fromMaybe ExposingAll
        <$> optional
            ( do
                token' TokenExposing
                ExposingSome <$> oneOrCommaSeparatedInParens exposition
            )

exposition :: HParser (Exposition Frontend)
exposition = exposedValue <|> exposedOp
  where
    exposedValue, exposedOp :: HParser (Exposition Frontend)
    exposedValue = ExposedValue <$> located varName
    exposedOp = ExposedOp <$> located (inParens (maybeQualified opName))

import' :: HParser (Import Frontend)
import' = fmapLocated Import $ do
    token' TokenImport
    endHead
    moduleName' <- located Parse.moduleName
    isQualified <- isJust <$> optional (token' TokenQualified)
    as <- optional . located $ do
        token' TokenAs
        Parse.moduleName
    Import' moduleName' as isQualified <$> exposing
