{-# LANGUAGE DisambiguateRecordFields #-}

module Elara.Parse.Module where

import Elara.AST.Module (Exposing (..), Exposition (..), Import (..), Import' (..), Module (..), Module' (..))
import Elara.AST.Name
import Elara.AST.Phases.Frontend (Frontend)
import Elara.AST.Region
import Elara.Lexer.Token (Token (..))
import Elara.Parse.Declaration (declaration)
import Elara.Parse.Indents (lineSeparator)
import Elara.Parse.Names (opName, varName)
import Elara.Parse.Names qualified as Parse (moduleName)
import Elara.Parse.Primitives
import Text.Megaparsec (getSourcePos, sepEndBy)

module' :: Parser (Module SourceRegion Frontend)
module' = do
    Located loc inner <- located $ do
        startPos <- getSourcePos
        _ <- optional lineSeparator
        mHeader <- optional (header <* optional lineSeparator)
        let _name = maybe (Located (RealSourceRegion $ mkSourceRegion startPos startPos) (ModuleName ("Main" :| []))) fst mHeader
        imports <- sepEndBy import' lineSeparator

        declarations <- sepEndBy (declaration _name) lineSeparator

        pure $
            Module'
                { moduleName = _name
                , moduleExposing = maybe ExposingAll snd mHeader
                , moduleImports = imports
                , moduleDeclarations = declarations
                }
    pure $ Module loc inner

-- | module Name exposing (..)
header :: Parser (Located ModuleName, Exposing SourceRegion Frontend)
header = do
    token_ TokenModule

    moduleName' <- located Parse.moduleName
    mExposing <- exposing'
    pure (moduleName', mExposing)

exposing' :: Parser (Exposing SourceRegion Frontend)
exposing' =
    fromMaybe ExposingAll
        <$> optional
            ( do
                token_ TokenExposing
                ExposingSome <$> oneOrCommaSeparatedInParens exposition
            )

exposition :: Parser (Exposition SourceRegion Frontend)
exposition = exposedValue <|> exposedOp
  where
    exposedValue, exposedOp :: Parser (Exposition SourceRegion Frontend)
    exposedValue = ExposedValue <$> located varName
    exposedOp = ExposedOp <$> located (inParens opName)

import' :: Parser (Import SourceRegion Frontend)
import' = do
    Located loc inner <- located $ do
        token_ TokenImport

        moduleName' <- located Parse.moduleName
        isQualified <- isJust <$> optional (token_ TokenQualified)
        as <- optional . located $ do
            token_ TokenAs
            Parse.moduleName
        Import' moduleName' as isQualified <$> exposing'
    pure $ Import loc inner
