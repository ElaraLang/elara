{-# LANGUAGE DisambiguateRecordFields #-}

module Elara.Parse.Module where

import Elara.AST.Module (Exposing (..), Exposition (ExposedOp, ExposedValue), Import (..), Import' (..), Module (..), Module' (..))
import Elara.AST.Name
import Elara.AST.Region
import Elara.AST.Select
import Elara.Lexer.Token (Token (..))
import Elara.Parse.Declaration (declaration)
import Elara.Parse.Indents (lineSeparator)
import Elara.Parse.Names (opName, varName)
import Elara.Parse.Names qualified as Parse (moduleName)
import Elara.Parse.Primitives
import Text.Megaparsec (getSourcePos, sepEndBy)

module' :: Parser (Module Frontend)
module' = fmapLocated Module $ do
    startPos <- getSourcePos
    _ <- optional lineSeparator -- the lexer sometimes emits a leading line separator
    -- especially when there are comments at the top of the file
    mHeader <- optional (header <* optional lineSeparator)
    let _name = maybe (Located (RealSourceRegion $ mkSourceRegion startPos startPos) (ModuleName ("Main" :| []))) fst mHeader
    imports <- sepEndBy import' lineSeparator

    declarations <- sepEndBy (declaration _name) lineSeparator

    pure $
        Module'
            { name = _name
            , exposing = maybe ExposingAll snd mHeader
            , imports = imports
            , declarations = declarations
            }

-- | module Name exposing (..)
header :: Parser (Located ModuleName, Exposing Frontend)
header = do
    token_ TokenModule

    moduleName' <- located Parse.moduleName
    mExposing <- exposing'
    pure (moduleName', mExposing)

exposing' :: Parser (Exposing Frontend)
exposing' =
    fromMaybe ExposingAll
        <$> optional
            ( do
                token_ TokenExposing
                ExposingSome <$> oneOrCommaSeparatedInParens exposition
            )

exposition :: Parser (Exposition Frontend)
exposition = exposedValue <|> exposedOp
  where
    exposedValue, exposedOp :: Parser (Exposition Frontend)
    exposedValue = ExposedValue <$> located varName
    exposedOp = ExposedOp <$> located (inParens opName)

import' :: Parser (Import Frontend)
import' = fmapLocated Import $ do
    token_ TokenImport

    moduleName' <- located Parse.moduleName
    isQualified <- isJust <$> optional (token_ TokenQualified)
    as <- optional . located $ do
        token_ TokenAs
        Parse.moduleName
    Import' moduleName' as isQualified <$> exposing'
