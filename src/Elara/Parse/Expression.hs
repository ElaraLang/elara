module Elara.Parse.Expression where

import Control.Monad.Combinators.Expr (Operator (InfixL, InfixR), makeExprParser)
import Data.Set qualified as Set
import Data.Text (Text)
import Elara.AST.Frontend (Expr, LocatedExpr)
import Elara.AST.Frontend qualified as Ast
import Elara.Data.Located as Located (merge)
import Elara.Data.Name qualified as Name
import Elara.Parse.Indents (optionallyIndented)
import Elara.Parse.Literal (charLiteral, floatLiteral, integerLiteral, stringLiteral)
import Elara.Parse.Name (opName, typeName, varName)
import Elara.Parse.Pattern (pattern)
import Elara.Parse.Primitives (Parser, inParens, lexeme, located, sc, symbol)
import Text.Megaparsec (MonadParsec (try), sepBy, (<|>))

-- import Text.Megaparsec (sepBy, try, (<|>))

expression :: Parser LocatedExpr
expression =
  makeExprParser
    expressionTerm
    [ [InfixR (Located.merge Ast.FunctionCall <$ sc)],
      [InfixL (Located.merge . Ast.BinaryOperator <$> operator)]
    ]

expressionTerm :: Parser LocatedExpr
expressionTerm =
  inParens expression
    <|> ifElse
    <|> lambda
    <|> int
    <|> try float
    <|> string
    <|> char
    <|> try variable
    <|> constructor

reservedWords ::
  -- | Reserved words, used to backtrack accordingly
  Set.Set Text
reservedWords = Set.fromList ["if", "else", "then", "let"]

variable :: Parser LocatedExpr
variable = located $ do
  var <- varName
  if Name.nameValue var `Set.member` reservedWords
    then fail "Reserved keyword"
    else return $ Ast.Var var

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
  (args, res) <- optionallyIndented lambdaPreamble expression
  return $ Ast.Lambda args res
  where
    lambdaPreamble = do
      symbol "\\"
      args <- lexeme (sepBy pattern sc)
      symbol "->"
      return args

promoteArguments :: [Name.Name] -> Expr -> Expr
promoteArguments allArgs arg = case arg of
  Ast.Var v ->
    case Name.moduleName v of
      Nothing -> if v `elem` allArgs then Ast.Argument v else arg
      _ -> arg
  _ -> arg

ifElse :: Parser LocatedExpr
ifElse = located $ do
  symbol "if"
  condition <- expression
  symbol "then"
  thenBranch <- expression
  symbol "else"
  Ast.If condition thenBranch <$> expression