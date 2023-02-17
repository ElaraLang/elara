module Elara.Parse.Module where

import Elara.AST.Module (Exposing (..), Exposition (ExposedOp, ExposedValue), Import (..), Module (..))
import Elara.AST.Name
import Elara.AST.Select
import Elara.Parse.Combinators (sepEndBy')
import Elara.Parse.Declaration (declaration)
import Elara.Parse.Names (opName, varName)
import Elara.Parse.Names qualified as Parse (moduleName)
import Elara.Parse.Primitives
import Text.Megaparsec (MonadParsec (try), sepEndBy)

module' :: Parser (Module Frontend)
module' = do
    mHeader <- optional . try $ header
    let _name = maybe (ModuleName ("Main" :| [])) fst mHeader
    skipNewlines
    imports <- import' `sepEndBy` skipNewlines
    declarations <- declaration _name `sepEndBy'` skipNewlines

    pure $
        Module
            { _moduleName = _name
            , _moduleExposing = maybe ExposingAll snd mHeader
            , _moduleImports = imports
            , _moduleDeclarations = declarations
            }

header :: Parser (ModuleName, Exposing MaybeQualified)
header = do
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
exposition = exposedValue <|> exposedOp
  where
    exposedValue = ExposedValue <$> varName
    exposedOp = ExposedOp <$> inParens opName

import' :: Parser (Import MaybeQualified)
import' = do
    symbol "import"
    moduleName' <- lexeme Parse.moduleName
    isQualified <- isJust <$> optional (symbol "qualified")
    as <- optional . try $ do
        symbol "as"
        lexeme Parse.moduleName
    Import moduleName' as isQualified <$> exposing