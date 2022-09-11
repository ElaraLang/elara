module Elara.Parse.Expression where

import Control.Lens.Plated (transform)
import Control.Monad.Combinators.Expr (Operator (InfixL, InfixR), makeExprParser)
import Data.Set qualified as Set
import Elara.AST.Frontend (LocatedExpr)
import Elara.AST.Frontend qualified as Ast
import Elara.AST.Generic (patternNames)
import Elara.Data.Located as Located (merge)
import Elara.Data.Name qualified as Name
import Elara.Parse.Indents (optionallyIndented, withCurrentIndentOrNormal, withIndentOrNormal)
import Elara.Parse.Literal (charLiteral, floatLiteral, integerLiteral, stringLiteral)
import Elara.Parse.Name (opName, typeName, varName)
import Elara.Parse.Name qualified as Name
import Elara.Parse.Pattern (pattern')
import Elara.Parse.Primitives (Parser, inParens, lexeme, located, sc, symbol)
import Text.Megaparsec (MonadParsec (try), sepBy, sepEndBy, (<?>))
import Text.Megaparsec.Char.Lexer (indentLevel)

exprParser :: Parser LocatedExpr
exprParser =
  makeExprParser
    expression
    [ [InfixR (Located.merge Ast.FunctionCall <$ sc)],
      [InfixL (Located.merge . Ast.BinaryOperator <$> operator)]
    ]
    <?> "expression"

-- | A top level element, comprised of either an expression or a let declaration
-- Note that top level let declarations are not parsed here, but in the "Elara.Parse.Declaration" module
element :: Parser LocatedExpr
element =
  try exprParser <|> try statement

expression :: Parser LocatedExpr
expression =
  inParens exprParser
    <|> letInExpression
    <|> ifElse
    <|> lambda
    <|> try float
    <|> int
    <|> string
    <|> char
    <|> try variable
    <|> constructor
    <|> list

statement :: Parser LocatedExpr
statement = letExpression <?> "statement"

-- | Reserved words, used to backtrack accordingly
reservedWords :: Set.Set Text
reservedWords = Set.fromList ["if", "else", "then", "let", "in"]

variable :: Parser LocatedExpr
variable = located $ do
  var <- varName
  if Name.nameValue var `Set.member` reservedWords
    then fail "Reserved keyword"
    else pure $ Ast.Var var

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
  (args, res) <- optionallyIndented lambdaPreamble element
  let argNames = args >>= patternNames
  pure (Name.promoteAll argNames (Ast.Lambda args res))
  where
    lambdaPreamble = do
      symbol "\\"
      args <- lexeme (sepBy (lexeme pattern') sc)
      symbol "->"
      pure args

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
  pure $ Ast.List elements

letExpression :: Parser LocatedExpr -- TODO merge this, Declaration.valueDecl, and letInExpression into 1 tidier thing
letExpression = located $ do
  ((name, patterns), e) <- optionallyIndented letPreamble element
  let names = patterns >>= patternNames
  let promote = fmap (transform (Name.promoteArguments names))

  pure (Ast.Let name patterns (promote e))
  where
    letPreamble = do
      symbol "let"
      name <- lexeme varName
      patterns <- sepBy (lexeme pattern') sc
      symbol "="
      pure (name, patterns)

letInExpression :: Parser LocatedExpr -- TODO merge this, Declaration.valueDecl, and letInExpression into 1 tidier thing
letInExpression = located $ do
  start <- indentLevel
  symbol "let"
  name <- varName -- we cant use a lexeme here or it'll push the current indent level too far forwards, breaking withCurrentIndentOrNormal
  (n, patterns) <- withCurrentIndentOrNormal $ do
    sc -- consume the spaces from the non-lexeme'd varName.
    sepBy (lexeme pattern') sc

  _ <- withIndentOrNormal n (symbol "=")
  (_, e) <- withIndentOrNormal n element

  _ <- withIndentOrNormal start (symbol "in")

  (_, body) <- withIndentOrNormal start element

  let names = patterns >>= patternNames
  let promote = fmap (transform (Name.promoteArguments names))
  pure (Ast.LetIn name patterns (promote e) body)
