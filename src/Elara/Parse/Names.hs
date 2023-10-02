module Elara.Parse.Names where

import Elara.AST.Name (LowerAlphaName (..), MaybeQualified (..), ModuleName (..), OpName (..), TypeName (..), VarName (NormalVarName, OperatorVarName), VarOrConName (..))
import Elara.Lexer.Token
import Elara.Parse.Primitives (Parser, inParens, satisfyMap)

moduleName :: Parser ModuleName
moduleName = satisfyMap $ \case
    TokenQConstructorIdentifier (ModuleName m, n) -> Just $ ModuleName (m <> one n)
    TokenConstructorIdentifier n -> Just $ ModuleName (one n)
    _ -> Nothing

varOrConName :: Parser (MaybeQualified VarOrConName)
varOrConName = (ConName <<$>> conName) <|> (VarName <<$>> normalVarName)

varName :: Parser (MaybeQualified VarName)
varName = operatorVarName' <|> normalVarName'
  where
    operatorVarName' = OperatorVarName <<$>> inParens opName
    normalVarName' = NormalVarName <<$>> normalVarName

unqualifiedVarName :: Parser VarName
unqualifiedVarName = operatorVarName' <|> normalVarName'
  where
    operatorVarName' = OperatorVarName <$> inParens opId
    normalVarName' = NormalVarName <$> varId

normalVarName :: Parser (MaybeQualified LowerAlphaName)
normalVarName = qVarId <|> (`MaybeQualified` Nothing) <$> varId

conName :: Parser (MaybeQualified TypeName)
conName = qConId <|> (`MaybeQualified` Nothing) <$> conId

opName :: Parser (MaybeQualified OpName)
opName = qOpId <|> (`MaybeQualified` Nothing) <$> opId

-- Qualified identifiers

qVarId :: Parser (MaybeQualified LowerAlphaName)
qVarId = satisfyMap $ \case
    TokenQVariableIdentifier (m, v) -> Just $ MaybeQualified (LowerAlphaName v) (Just m)
    _ -> Nothing

qConId :: Parser (MaybeQualified TypeName)
qConId = satisfyMap $ \case
    TokenQConstructorIdentifier (m, v) -> Just $ MaybeQualified (TypeName v) (Just m)
    _ -> Nothing

qOpId :: Parser (MaybeQualified OpName)
qOpId = satisfyMap $ \case
    TokenQOperatorIdentifier (m, v) -> Just $ MaybeQualified (OpName v) (Just m)
    _ -> Nothing

-- Unqualified identifiers

varId :: Parser LowerAlphaName
varId = satisfyMap $ \case
    TokenVariableIdentifier v -> Just $ LowerAlphaName v
    _ -> Nothing

conId :: Parser TypeName
conId = satisfyMap $ \case
    TokenConstructorIdentifier v -> Just $ TypeName v
    _ -> Nothing

opId :: Parser OpName
opId = satisfyMap $ \case
    TokenOperatorIdentifier v -> Just $ OpName v
    _ -> Nothing
