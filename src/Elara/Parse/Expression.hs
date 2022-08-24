module Elara.Parse.Expression where

import Control.Lens.Plated (transform)
import Control.Monad.Combinators.Expr (Operator (InfixL, InfixR), makeExprParser)
import Data.Set qualified as Set
import Data.Text (Text)
import Elara.AST.Frontend (LocatedExpr)
import Elara.AST.Frontend qualified as Ast
import Elara.AST.Generic (patternNames)
import Elara.Data.Located as Located (merge)
import Elara.Data.Module (mapExpr)
import Elara.Data.Name qualified as Name
import Elara.Parse.Indents (optionallyIndented)
import Elara.Parse.Literal (charLiteral, floatLiteral, integerLiteral, stringLiteral)
import Elara.Parse.Name (opName, promoteArguments, typeName, varName)
import Elara.Parse.Name qualified as Name
import Elara.Parse.Pattern (pattern')
import Elara.Parse.Primitives (Parser, inParens, lexeme, located, sc, symbol)
import Text.Megaparsec (MonadParsec (try), sepBy, sepEndBy, (<?>), (<|>))
import Text.Megaparsec.Debug (dbg)

exprParser :: Parser LocatedExpr
exprParser =
  makeExprParser
    expressionTerm
    [ [InfixR (Located.merge Ast.FunctionCall <$ sc)],
      [InfixL (Located.merge . Ast.BinaryOperator <$> operator)]
    ]
    <?> "expression"

expression :: Parser LocatedExpr
expression =
  try
    ( statement <|> dbg "exp" exprParser
    )

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
    <|> list
    <|> try letInExpression

statement :: Parser LocatedExpr
statement = dbg "statement" (letExpression <?> "statement")

-- | Reserved words, used to backtrack accordingly
reservedWords :: Set.Set Text
reservedWords = Set.fromList ["if", "else", "then", "let", "in"]

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
  let argNames = args >>= patternNames
  return $ transform (promoteArguments argNames) (Ast.Lambda args res)
  where
    lambdaPreamble = do
      symbol "\\"
      args <- lexeme (sepBy pattern' sc)
      symbol "->"
      return args

ifElse :: Parser LocatedExpr
ifElse = located $ do
  symbol "if"
  condition <- expression
  symbol "then"
  thenBranch <- expression
  symbol "else"
  Ast.If condition thenBranch <$> expression

list :: Parser LocatedExpr
list = located $ do
  symbol "["
  elements <- lexeme (sepEndBy expression (symbol ","))
  symbol "]"
  return $ Ast.List elements

letExpression :: Parser LocatedExpr -- TODO merge this, Declaration.valueDecl, and letInExpression into 1 tidier thing
letExpression = located $ do
  ((name, patterns), e) <- optionallyIndented letPreamble expression
  let names = patterns >>= patternNames
  let promote = fmap (transform (Name.promoteArguments names))
      exp = Ast.Let name patterns (promote e)

  return exp
  where
    letPreamble = do
      symbol "let"
      name <- varName
      patterns <- sepBy (lexeme pattern') sc
      symbol "="
      return (name, patterns)

letInExpression :: Parser LocatedExpr -- TODO merge this, Declaration.valueDecl, and letInExpression into 1 tidier thing
letInExpression = located $ do
  ((name, patterns), e) <- optionallyIndented letPreamble expression
  (_, body) <- optionallyIndented inPreamble expression
  let names = patterns >>= patternNames
  let promote = fmap (transform (Name.promoteArguments names))
      exp = Ast.LetIn name patterns (promote e) body

  return exp
  where
    letPreamble = do
      symbol "let"
      name <- varName
      patterns <- sepBy (lexeme pattern') sc
      symbol "="
      return (name, patterns)
    inPreamble = do
      symbol "in"