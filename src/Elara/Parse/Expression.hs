module Elara.Parse.Expression where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Set qualified as Set
import Elara.AST.Frontend (Expr (..))
import Elara.AST.Frontend qualified as Frontend
import Elara.AST.Name (MaybeQualified (..), VarName, nameText)
import Elara.AST.Region (Located (..), enclosingRegion, getLocation)
import Elara.Parse.Indents (blockAt, optionallyIndented, optionallyIndented', withCurrentIndentOrNormal, withIndentOrNormal)
import Elara.Parse.Literal (charLiteral, floatLiteral, integerLiteral, stringLiteral)
import Elara.Parse.Names (opName, typeName, varName)
import Elara.Parse.Pattern (pattern')
import Elara.Parse.Primitives (Parser, inParens, lexeme, located, sc, symbol)
import Text.Megaparsec (MonadParsec (try), sepBy, sepEndBy, (<?>))
import Text.Megaparsec.Char (char, space1)
import Text.Megaparsec.Char.Lexer (indentLevel)

locatedExpr :: Parser Frontend.Expr' -> Parser Expr
locatedExpr = (Expr <$>) . located

exprParser :: Parser Expr
exprParser =
    makeExprParser
        (try expression)
        [ [InfixL functionCall]
        , [InfixR binOp]
        ]
        <?> "expression"

{- | A top level element, comprised of either an expression or a let declaration
Note that top level let declarations are not parsed here, but in the "Elara.Parse.Declaration" module
-}
element :: Parser Expr
element =
    (try exprParser <|> try statement) <?> "element"

{- | This is not a "statement" in an imperative sense, but is used to parse let expressions that are not at the top level.
| The reason for having this distinction is that if let's were considered a normal expression, then something like
| @let x = let y = 1@ would be considered valid code, which it is not. Having a separate parser means local let bindings can only be allowed
| in places where they are valid, such as in the body of a function.
-}
statement :: Parser Expr
statement = letExpression <?> "statement"

-- Lift a binary operator to work on `Expr` instead of `Frontend.Expr`. Probably not the best way to do this, but it works
liftedBinary :: Monad m => m t -> (t -> Expr -> Expr -> Frontend.Expr') -> m (Expr -> Expr -> Expr)
liftedBinary op f = do
    op' <- op
    let create l'@(Expr l) r'@(Expr r) =
            let region = enclosingRegion (getLocation l) (getLocation r)
             in Expr $ Located region (f op' l' r')
    pure create

binOp, functionCall :: Parser (Expr -> Expr -> Expr)
binOp = liftedBinary operator Frontend.BinaryOperator
functionCall = liftedBinary sc (const Frontend.FunctionCall)

-- This isn't actually used in `expressionTerm` as `varName` also covers (+) operators, but this is used when parsing infix applications
operator :: Parser Frontend.BinaryOperator
operator = Frontend.MkBinaryOperator <$> (asciiOp <|> infixOp) <?> "operator"
  where
    asciiOp = located $ do
        Frontend.Op <$> opName
    infixOp = located $ lexeme $ do
        _ <- char '`'
        op <- varName
        _ <- char '`'
        pure $ Frontend.Infixed op

expression :: Parser Frontend.Expr
expression =
    unit
        <|> (try parensExpr <?> "parenthesized expression")
        <|> (ifElse <?> "if expression")
        <|> (letInExpression <?> "let-in expression")
        <|> (lambda <?> "lambda expression")
        <|> (try float <?> "float")
        <|> (int <?> "int")
        <|> (charL <?> "char")
        <|> (string <?> "string")
        <|> (variable <?> "variable")
        <|> (constructor <?> "constructor")
        <|> (list <?> "list")
        <?> "expression"

-- | Reserved words, used to backtrack accordingly
reservedWords :: Set Text
reservedWords = Set.fromList ["if", "else", "then", "let", "in"]

parensExpr :: Parser Frontend.Expr
parensExpr = do
    e@(Frontend.Expr le) <- inParens exprParser
    pure (Frontend.Expr (Frontend.InParens e <$ le))

variable :: Parser Frontend.Expr
variable = locatedExpr $ do
    var <- lexeme varName
    if nameText var `Set.member` reservedWords
        then fail ("Reserved keyword: " <> toString (nameText var))
        else pure $ Frontend.Var var

constructor :: Parser Frontend.Expr
constructor = locatedExpr $ do
    con <- lexeme typeName
    pure $ Frontend.Constructor con

unit :: Parser Frontend.Expr
unit = locatedExpr (Frontend.Unit <$ symbol "()") <?> "unit"

int :: Parser Frontend.Expr
int = locatedExpr (Frontend.Int <$> integerLiteral) <?> "int"

float :: Parser Frontend.Expr
float = locatedExpr (Frontend.Float <$> floatLiteral) <?> "float"

string :: Parser Frontend.Expr
string = locatedExpr (Frontend.String <$> stringLiteral) <?> "string"

charL :: Parser Frontend.Expr
charL = locatedExpr (Frontend.Char <$> charLiteral) <?> "char"

list :: Parser Frontend.Expr
list = locatedExpr $ do
    symbol "["
    elements <- lexeme (sepEndBy expression (symbol ","))
    symbol "]"
    pure $ Frontend.List elements

lambda :: Parser Expr
lambda = locatedExpr $ do
    start <- indentLevel
    args <- lambdaPreamble
    (_, res) <- blockAt start element
    pure (Frontend.Lambda args res)
  where
    lambdaPreamble = do
        symbol "\\"
        args <- lexeme (sepBy (lexeme pattern') sc)
        symbol "->"
        pure args

ifElse :: Parser Expr
ifElse = locatedExpr $ do
    condition <- optionallyIndented' (symbol "if") element
    _ <- many space1 -- scn doesn't work because it succeeds on an empty input
    thenBranch <- optionallyIndented' (symbol "then") element
    _ <- many space1
    elseBranch <- optionallyIndented' (symbol "else") element
    _ <- many space1
    pure (Frontend.If condition thenBranch elseBranch)

letExpression :: Parser Expr -- TODO merge this, Declaration.valueDecl, and letInExpression into 1 tidier thing
letExpression = locatedExpr $ do
    (name, patterns, e) <- letRaw
    -- let names = patterns >>= patternNames
    -- let promote = fmap (transform (Name.promoteArguments names))
    pure (Frontend.Let name patterns e)

letInExpression :: Parser Frontend.Expr -- TODO merge this, Declaration.valueDecl, and letInExpression into 1 tidier thing
letInExpression = locatedExpr $ do
    start <- indentLevel
    symbol "let"
    name <- varName -- cant use a lexeme here or it'll push the current indent level too far forwards, breaking withCurrentIndentOrNormal
    afterName <- indentLevel
    (afterPatterns, patterns) <- withCurrentIndentOrNormal $ do
        sc -- consume the spaces from the non-lexeme'd varName.
        sepBy (lexeme pattern') sc

    _ <- withIndentOrNormal afterPatterns (symbol "=")
    (_, e) <- blockAt afterName element

    _ <- withIndentOrNormal start (symbol "in")

    (_, body) <- blockAt start element

    -- let names = patterns >>= patternNames
    -- let promote = fmap (transform (Name.promoteArguments names))
    pure (Frontend.LetIn name patterns e body)

letRaw :: Parser (MaybeQualified VarName, [Frontend.Pattern], Frontend.Expr)
letRaw = do
    ((name, patterns), e) <- optionallyIndented letPreamble element
    pure (name, patterns, e)
  where
    letPreamble = do
        symbol "let"
        name <- lexeme varName
        patterns <- sepBy (lexeme pattern') sc
        symbol "="
        pure (name, patterns)
