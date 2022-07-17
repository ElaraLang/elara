module Elara.Parse.Expression where

import Control.Monad.Combinators.Expr (Operator (InfixL, InfixR), makeExprParser)
import Elara.AST.Frontend (LocatedExpr)
import Elara.AST.Frontend qualified as Ast
import Elara.Data.Located as Located (merge)
import Elara.Parse.Literal (charLiteral, floatLiteral, integerLiteral, stringLiteral)
import Elara.Parse.Name (opName, typeName, varName)
import Elara.Parse.Primitives (Parser, inParens, lexeme, located, sc)
import Text.Megaparsec (sepBy, try)
import Text.Megaparsec.Char qualified as C
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
            lambda,
            ifElse,
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

lambda :: Parser LocatedExpr
lambda = located $ do
  _ <- lexeme (C.char '\\')
  args <- lexeme (sepBy varName sc)
  _ <- lexeme (C.string "->")
  Ast.Lambda args <$> expression

ifElse :: Parser LocatedExpr
ifElse = located $ do
  _ <- lexeme (C.string "if")
  cond <- expression
  _ <- lexeme (C.string "then")
  thenBranch <- expression
  _ <- lexeme (C.string "else")
  elseBranch <- expression
  return $ Ast.If cond thenBranch elseBranch