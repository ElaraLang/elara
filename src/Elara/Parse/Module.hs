module Elara.Parse.Module where

import Elara.AST.Module (Exposing (..), Exposition (ExposedValue), Import (..), Module (..))
import Elara.AST.Name
import Elara.AST.Select
import Elara.Parse.Declaration (declaration)
import Elara.Parse.Names (varName)
import Elara.Parse.Names qualified as Parse (moduleName)
import Elara.Parse.Primitives
import Text.Megaparsec (MonadParsec (try), sepEndBy)
import Text.Megaparsec.Char (newline)

module' :: Parser (Module Frontend)
module' = do
    header <- parseHeader
    let _name = maybe (ModuleName ("Main" :| [])) fst header
    _ <- many newline

    imports <- import' `sepEndBy` many newline
    declarations <- declaration _name `sepEndBy` many newline

    pure $
        Module
            { _moduleName = _name
            , _moduleExposing = maybe ExposingAll snd header
            , _moduleImports = imports
            , _moduleDeclarations = declarations
            }

parseHeader :: Parser (Maybe (ModuleName, Exposing MaybeQualified))
parseHeader = optional . try $ do
    -- module Name exposing (..)
    symbol "module"
    moduleName' <- lexeme Parse.moduleName
    exposing' <- exposing
    pure (moduleName', exposing')

exposing :: Parser (Exposing MaybeQualified)
exposing =
    fromMaybe ExposingAll
        <$> ( optional . try $ do
                symbol "exposing"
                es <- lexeme (oneOrCommaSeparatedInParens exposition)
                pure $ ExposingSome es
            )

exposition :: Parser (Exposition MaybeQualified)
exposition = ExposedValue <$> varName

import' :: Parser (Import MaybeQualified)
import' = do
    symbol "import"
    moduleName' <- lexeme Parse.moduleName
    isQualified <- isJust <$> optional (symbol "qualified")
    as <- optional . try $ do
        symbol "as"
        lexeme Parse.moduleName
    Import moduleName' as isQualified <$> exposing