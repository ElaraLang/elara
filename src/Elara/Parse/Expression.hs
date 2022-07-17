module Elara.Parse.Expression where

import Control.Monad.Combinators.Expr (Operator (InfixL, InfixR), makeExprParser)
import Elara.AST.Frontend (LocatedExpr)
import Elara.AST.Frontend qualified as Ast
import Elara.Data.Located as Located (merge)
import Elara.Data.Name qualified as Name
import Elara.Parse.Literal (charLiteral, floatLiteral, integerLiteral, stringLiteral)
import Elara.Parse.Name (opName, typeName, varName)
import Elara.Parse.Primitives (Parser, inParens, located, sc)
import Text.Megaparsec (try)
import Text.Parser.Combinators (choice)

expression :: Parser LocatedExpr
expression =
  makeExprParser
    expressionTerm
    [ [InfixR ((Located.merge Ast.FunctionCall) <$ sc)],
      [InfixL ((Located.merge . Ast.BinaryOperator) <$> operator)]
    ]

expressionTerm :: Parser LocatedExpr
expressionTerm =
  choice $
    try
      <$> [ inParens expression,
            variable,
            constructor,
            int,
            float,
            char,
            string
          ]

variable :: Parser LocatedExpr
variable = located (Ast.Var <$> varName)

-- This isn't actually used in `expressionTerm` as `varName` also covers (+) operators, but this is used when parsing infix applications
operator :: Parser LocatedExpr
operator = located (Ast.Var <$> opName)

constructor :: Parser LocatedExpr
constructor = located (Ast.Constructor <$> typeName)

int :: Parser LocatedExpr
int = located (Ast.Int <$> integerLiteral)

float :: Parser LocatedExpr
float = located (Ast.Float <$> floatLiteral)

char :: Parser LocatedExpr
char = located (Ast.Char <$> charLiteral)

string :: Parser LocatedExpr
string = located (Ast.String <$> stringLiteral)