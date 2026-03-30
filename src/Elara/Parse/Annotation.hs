module Elara.Parse.Annotation where

import Elara.AST.Extensions (InParensExtension (..), ListExprExtension (..), TupleExprExtension (..))
import Elara.AST.Phases.Frontend
import Elara.AST.Region (SourceRegion)
import Elara.AST.Types
import Elara.Lexer.Token (Token (..))
import Elara.Parse.Error (ElaraParseError (..))
import Elara.Parse.Expression (exprParser)
import Elara.Parse.Indents (lineSeparator)
import Elara.Parse.Names (conName)
import Elara.Parse.Primitives
import Text.Megaparsec (customFailure)

annotations :: Parser [Annotation SourceRegion Frontend]
annotations = many annotation

annotation :: Parser (Annotation SourceRegion Frontend)
annotation = do
    token_ TokenHash
    annName <- located conName

    args <- many constExpr

    void $ optional lineSeparator -- if the annotation ends with a new line
    pure (Annotation annName args)

constExpr :: Parser (AnnotationArg SourceRegion Frontend)
constExpr = do
    e <- exprParser
    validateConstExpr e

validateConstExpr :: FrontendExpr -> Parser (AnnotationArg SourceRegion Frontend)
validateConstExpr e = validateConstExpr' e
  where
    validateConstExpr' :: FrontendExpr -> Parser (AnnotationArg SourceRegion Frontend)
    validateConstExpr' focus@(Expr _ _ x) = validateConstExpr'' focus x

    validateConstExpr'' :: FrontendExpr -> FrontendExpr' -> Parser (AnnotationArg SourceRegion Frontend)
    validateConstExpr'' var (EVar _ _) = customFailure (InvalidConstantExpression e var)
    validateConstExpr'' lam (ELam{}) = customFailure (InvalidConstantExpression e lam)
    validateConstExpr'' if' (EIf{}) = customFailure (InvalidConstantExpression e if')
    validateConstExpr'' match (EMatch{}) = customFailure (InvalidConstantExpression e match)
    validateConstExpr'' let' (ELet{}) = customFailure (InvalidConstantExpression e let')
    validateConstExpr'' let' (ELetIn{}) = customFailure (InvalidConstantExpression e let')
    validateConstExpr'' block (EBlock{}) = customFailure (InvalidConstantExpression e block)
    validateConstExpr'' app (ETyApp{}) = customFailure (InvalidConstantExpression e app)
    validateConstExpr'' int (EInt{}) = pure (AnnotationArg int)
    validateConstExpr'' float (EFloat{}) = pure (AnnotationArg float)
    validateConstExpr'' string (EString{}) = pure (AnnotationArg string)
    validateConstExpr'' char (EChar{}) = pure (AnnotationArg char)
    validateConstExpr'' unit EUnit{} = pure (AnnotationArg unit)
    validateConstExpr'' con (ECon{}) = pure (AnnotationArg con)
    validateConstExpr'' func (EApp _ a b) = do
        void $ validateConstExpr' a
        void $ validateConstExpr' b
        pure (AnnotationArg func)
    validateConstExpr'' ext (EExtension extExpr) = case extExpr of
        FrontendBinaryOperator{} -> customFailure (InvalidConstantExpression e ext)
        FrontendMultiLam{} -> customFailure (InvalidConstantExpression e ext)
        FrontendLetWithPatterns{} -> customFailure (InvalidConstantExpression e ext)
        FrontendLetInWithPatterns{} -> customFailure (InvalidConstantExpression e ext)
        FrontendInParens (InParensExpression inner) -> do
            void $ validateConstExpr' inner
            pure (AnnotationArg ext)
        FrontendList (ListExpression elems) -> do
            traverse_ validateConstExpr' elems
            pure (AnnotationArg ext)
        FrontendTuple (TupleExpression elems) -> do
            traverse_ validateConstExpr' (toList elems)
            pure (AnnotationArg ext)
    validateConstExpr'' ann (EAnn{}) = customFailure (InvalidConstantExpression e ann)
