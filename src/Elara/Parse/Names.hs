module Elara.Parse.Names where

import Data.Text qualified as Text
import Elara.AST.Name (LowerAlphaName (..), MaybeQualified (..), ModuleName (..), OpName (..), TypeName (..), VarName (..), _OpName)
import Elara.Lexer.Token
import Elara.Parse.Combinators (sepBy1')
import Elara.Parse.Primitives (HParser, inParens, satisfyMap, token_, (<??>))
import HeadedMegaparsec (endHead)
import Control.Lens (concatMapOf, concatOf, foldOf, folded, folding, toListOf, asumOf, Each (each), foldMapOf)

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

unqualifiedTypeName :: HParser TypeName
unqualifiedTypeName = TypeName <$> upperVarName

maybeQualified :: HParser name -> HParser (MaybeQualified name)
maybeQualified nameParser = qualified <|> unqualified
  where
    unqualified = MaybeQualified <$> nameParser <*> pure Nothing
    qualified = do
        qual <- moduleName
        token_ TokenDot
        endHead
        MaybeQualified <$> nameParser <*> pure (Just qual)

moduleName :: HParser ModuleName
moduleName = ModuleName <$> sepBy1' upperVarName (token_ TokenDot)

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
opName =do
    o <- some normal
    pure $ OpName $ foldOf (each . _OpName) o
  where
    normal :: HParser OpName
    normal =
        satisfyMap $ \case
            TokenDoubleColon -> Just "::"
            TokenOperatorIdentifier i -> Just (OpName i)
            TokenDot -> Just "."
            _ -> Nothing
