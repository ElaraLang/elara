module Elara.Parse.Module where

import Elara.AST.Module (Exposing (..), Exposition (ExposedOp, ExposedValue), Import (..), Module (..), Module' (..))
import Elara.AST.Name
import Elara.AST.Select
import Elara.Parse.Declaration (declaration)
import Elara.Parse.Names (opName, varName)
import Elara.Parse.Names qualified as Parse (moduleName)
import Elara.Parse.Primitives
import HeadedMegaparsec (endHead)
import Text.Megaparsec (sepBy, sepEndBy)

module' :: HParser (Module Frontend)
module' = do
    mHeader <- optional header
    let _name = maybe (ModuleName ("Main" :| [])) fst mHeader
    skipNewlines
    imports <- import' `sepEndBy` skipNewlines
    declarations <- declaration _name `sepBy` skipNewlines

    pure $
        Module'
            { _module'Name = _name
            , _module'Exposing = maybe ExposingAll snd mHeader
            , _module'Imports = imports
            , _module'Declarations = declarations
            }

header :: HParser (ModuleName, Exposing MaybeQualified)
header = do
    -- module Name exposing (..)
    symbol "module"
    endHead
    moduleName' <- lexeme Parse.moduleName
    exposing' <- exposing
    pure (moduleName', exposing')

exposing :: HParser (Exposing MaybeQualified)
exposing =
    fromMaybe ExposingAll
        <$> optional
            ( do
                symbol "exposing"
                es <- lexeme (oneOrCommaSeparatedInParens exposition)
                pure $ ExposingSome es
            )

exposition :: HParser (Exposition MaybeQualified)
exposition = exposedValue <|> exposedOp
  where
    exposedValue = ExposedValue <$> varName
    exposedOp = ExposedOp <$> inParens opName

import' :: HParser (Import MaybeQualified)
import' = do
    symbol "import"
    endHead
    moduleName' <- located $ lexeme Parse.moduleName
    isQualified <- isJust <$> optional (symbol "qualified")
    as <- located . optional $ do
        symbol "as"
        lexeme Parse.moduleName
    Import moduleName' (sequenceA as) isQualified <$> exposing