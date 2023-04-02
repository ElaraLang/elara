module Elara.Parse.Names where

import Elara.AST.Name (LowerAlphaName (..), MaybeQualified (..), ModuleName (..), OpName (..), TypeName (..), VarName (..))
import Elara.Lexer.Token
import Elara.Parse.Combinators (sepBy1')
import Elara.Parse.Primitives (HParser, inParens, satisfyMap, token', (<??>))
import HeadedMegaparsec (endHead)

varName :: HParser (MaybeQualified VarName)
varName = operatorVarName <|> normalVarName

unqualifiedVarName :: HParser VarName
unqualifiedVarName = unqualifiedOperatorVarName <|> unqualifiedNormalVarName

normalVarName :: HParser (MaybeQualified VarName)
normalVarName = maybeQualified $ unqualifiedNormalVarName <??> "variable name"

unqualifiedNormalVarName :: HParser VarName
unqualifiedNormalVarName = NormalVarName <$> alphaVarName <??> "variable name"

operatorVarName :: HParser (MaybeQualified VarName)
operatorVarName = (OperatorVarName <<$>> inParens (maybeQualified opName)) <??> "operator name in parens"

unqualifiedOperatorVarName :: HParser VarName
unqualifiedOperatorVarName = (OperatorVarName <$> inParens opName) <??> "operator name in parens"

typeName :: HParser (MaybeQualified TypeName)
typeName = do
    ModuleName names <- moduleName
    pure $ case names of
        x :| [] -> MaybeQualified (TypeName x) Nothing
        _ -> MaybeQualified (TypeName (last names)) (Just $ ModuleName (fromList $ init names))

maybeQualified :: HParser name -> HParser (MaybeQualified name)
maybeQualified nameParser = unqualified <|> qualified
  where
    unqualified = MaybeQualified <$> nameParser <*> pure Nothing
    qualified = do
        qual <- moduleName
        endHead
        token' TokenDot
        MaybeQualified <$> nameParser <*> pure (Just qual)

moduleName :: HParser ModuleName
moduleName = ModuleName <$> sepBy1' upperVarName (token' TokenDot)

upperVarName :: HParser Text
upperVarName = satisfyMap $
    \case
        TokenConstructorIdentifier i -> Just i
        _ -> Nothing

alphaVarName :: HParser LowerAlphaName
alphaVarName =
    LowerAlphaName
        <$> satisfyMap
            ( \case
                TokenVariableIdentifier i -> Just i
                _ -> Nothing
            )

opName :: HParser OpName
opName = satisfyMap $ \case
    TokenOperatorIdentifier i -> Just (OpName i)
    _ -> Nothing
