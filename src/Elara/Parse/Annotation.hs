module Elara.Parse.Annotation where

import Elara.AST.Frontend (FrontendExpr, FrontendExpr')
import Elara.AST.Generic.Types (Annotation (..), AnnotationArg (..), Expr (..), Expr' (..))
import Elara.AST.Region (Located (..))
import Elara.AST.Select
import Elara.Lexer.Token (Token (..))
import Elara.Parse.Error (ElaraParseError (..))
import Elara.Parse.Expression (expression)
import Elara.Parse.Indents (lineSeparator)
import Elara.Parse.Names (conName)
import Elara.Parse.Primitives
import Text.Megaparsec (customFailure)

annotations :: Parser [Annotation Frontend]
annotations = many annotation

annotation :: Parser (Annotation Frontend)
annotation = do
    token_ TokenHash
    annName <- located conName

    args <- many constExpr

    void $ optional lineSeparator -- if the annotation ends with a new line
    pure (Annotation annName args)

constExpr :: Parser (AnnotationArg Frontend)
constExpr = do
    e <- expression
    validateConstExpr e

validateConstExpr :: FrontendExpr -> Parser (AnnotationArg Frontend)
validateConstExpr e = validateConstExpr' e
  where
    validateConstExpr' :: FrontendExpr -> Parser (AnnotationArg Frontend)
    validateConstExpr' focus@(Expr (Located _ x, _)) = validateConstExpr'' focus x

    validateConstExpr'' :: FrontendExpr -> FrontendExpr' -> Parser (AnnotationArg Frontend)
    validateConstExpr'' var (Var _) = customFailure (InvalidConstantExpression e var)
    validateConstExpr'' lam (Lambda{}) = customFailure (InvalidConstantExpression e lam)
    validateConstExpr'' if' (If{}) = customFailure (InvalidConstantExpression e if')
    validateConstExpr'' bin (BinaryOperator{}) = customFailure (InvalidConstantExpression e bin)
    validateConstExpr'' match (Match{}) = customFailure (InvalidConstantExpression e match)
    validateConstExpr'' let' (Let{}) = customFailure (InvalidConstantExpression e let')
    validateConstExpr'' let' (LetIn{}) = customFailure (InvalidConstantExpression e let')
    validateConstExpr'' block (Block{}) = customFailure (InvalidConstantExpression e block)
    validateConstExpr'' app (TypeApplication{}) = customFailure (InvalidConstantExpression e app)
    validateConstExpr'' int (Int{}) = pure (AnnotationArg int)
    validateConstExpr'' float (Float{}) = pure (AnnotationArg float)
    validateConstExpr'' string (String{}) = pure (AnnotationArg string)
    validateConstExpr'' char (Char{}) = pure (AnnotationArg char)
    validateConstExpr'' unit Unit{} = pure (AnnotationArg unit)
    validateConstExpr'' con Constructor{} = pure (AnnotationArg con)
    validateConstExpr'' func (FunctionCall a b) = do
        void $ validateConstExpr' a
        void $ validateConstExpr' b
        pure (AnnotationArg func) -- just validating
    validateConstExpr'' list (List elems) = do
        traverse_ validateConstExpr' elems
        pure (AnnotationArg list)
    validateConstExpr'' tuple (Tuple elems) = do
        traverse_ validateConstExpr' elems
        pure (AnnotationArg tuple)
    validateConstExpr'' inParens (InParens e) = do
        void $ validateConstExpr' e
        pure (AnnotationArg inParens)
