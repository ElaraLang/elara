module Elara.Parse.Names where

import Elara.AST.Name (LowerAlphaName (..), MaybeQualified (..), ModuleName (..), OpName (..), TypeName (..), VarName (NormalVarName, OperatorVarName), VarOrConName (..))
import Elara.Lexer.Token
import Elara.Parse.Primitives (HParser, inParens, satisfyMap)

moduleName :: HParser ModuleName
moduleName = satisfyMap $ \case
    TokenQConstructorIdentifier (ModuleName m, n) -> Just $ ModuleName (m <> one n)
    TokenConstructorIdentifier n -> Just $ ModuleName (one n)
    _ -> Nothing

varOrConName :: HParser (MaybeQualified VarOrConName)
varOrConName = (ConName <<$>> conName) <|> (VarName <<$>> normalVarName)

varName :: HParser (MaybeQualified VarName)
varName = operatorVarName' <|> normalVarName'
  where
    operatorVarName' = OperatorVarName <<$>> inParens opName
    normalVarName' = NormalVarName <<$>> normalVarName

unqualifiedVarName :: HParser VarName
unqualifiedVarName = operatorVarName' <|> normalVarName'
  where
    operatorVarName' = OperatorVarName <$> inParens opId
    normalVarName' = NormalVarName <$> varId

normalVarName :: HParser (MaybeQualified LowerAlphaName)
normalVarName = qVarId <|> (`MaybeQualified` Nothing) <$> varId

conName :: HParser (MaybeQualified TypeName)
conName = qConId <|> (`MaybeQualified` Nothing) <$> conId

opName :: HParser (MaybeQualified OpName)
opName = qOpId <|> (`MaybeQualified` Nothing) <$> opId

-- Qualified identifiers

qVarId :: HParser (MaybeQualified LowerAlphaName)
qVarId = satisfyMap $ \case
    TokenQVariableIdentifier (m, v) -> Just $ MaybeQualified (LowerAlphaName v) (Just m)
    _ -> Nothing

qConId :: HParser (MaybeQualified TypeName)
qConId = satisfyMap $ \case
    TokenQConstructorIdentifier (m, v) -> Just $ MaybeQualified (TypeName v) (Just m)
    _ -> Nothing

qOpId :: HParser (MaybeQualified OpName)
qOpId = satisfyMap $ \case
    TokenQOperatorIdentifier (m, v) -> Just $ MaybeQualified (OpName v) (Just m)
    _ -> Nothing

-- Unqualified identifiers

varId :: HParser LowerAlphaName
varId = satisfyMap $ \case
    TokenVariableIdentifier v -> Just $ LowerAlphaName v
    _ -> Nothing

conId :: HParser TypeName
conId = satisfyMap $ \case
    TokenConstructorIdentifier v -> Just $ TypeName v
    _ -> Nothing

opId :: HParser OpName
opId = satisfyMap $ \case
    TokenOperatorIdentifier v -> Just $ OpName v
    _ -> Nothing
