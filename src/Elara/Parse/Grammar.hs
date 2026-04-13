{- | Ties the knot for the expression grammar,
allowing for mutually recursive parsers without risk of deadlocks 🥲
-}
module Elara.Parse.Grammar (exprParser, element, letPreambleParser) where

import Elara.AST.Name
import Elara.AST.Phases.Frontend
import Elara.AST.Region
import Elara.Parse.Expression qualified as Expr
import Elara.Parse.Primitives

grammarKnot :: Expr.ExpressionGrammar
grammarKnot =
    Expr.ExpressionGrammar
        { pElement = Expr.element grammarKnot
        , pExpression = Expr.expression grammarKnot
        }

exprParser :: Parser FrontendExpr
exprParser = grammarKnot.pExpression

element :: Parser FrontendExpr
element = grammarKnot.pElement

letPreambleParser :: Parser (Located VarName, [FrontendPattern], FrontendExpr)
letPreambleParser = Expr.letPreamble grammarKnot
