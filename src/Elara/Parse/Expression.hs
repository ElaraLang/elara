module Elara.Parse.Expression where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Set qualified as Set
import Elara.AST.Frontend (Expr (..))
import Elara.AST.Frontend qualified as Frontend
import Elara.AST.Name (MaybeQualified (..), Unqualified, VarName, nameText)
import Elara.AST.Region (Located (..), enclosingRegion, getLocation)
import Elara.Parse.Error
import Elara.Parse.Indents (blockAt, optionallyIndented, sub1, withCurrentIndentOrNormal, withIndentOrNormal)
import Elara.Parse.Literal (charLiteral, floatLiteral, integerLiteral, stringLiteral)
import Elara.Parse.Names (maybeQualified, opName, typeName, unqualifiedVarName, varName)
import Elara.Parse.Pattern (pattern')
import Elara.Parse.Primitives (HParser, IsParser (fromParsec), char', inParens, lexeme, located, sc, symbol, withPredicate, (<??>))
import HeadedMegaparsec (endHead)
import HeadedMegaparsec qualified as H (endHead, parse, toParsec)
import Text.Megaparsec (Pos, sepBy, sepEndBy)
import Text.Megaparsec.Char.Lexer (indentLevel)

locatedExpr :: HParser Frontend.Expr' -> HParser Expr
locatedExpr = (Expr <$>) . (H.parse . located . H.toParsec)

exprParser :: HParser Expr
exprParser =
    makeExprParser
        expression
        [ [InfixL functionCall]
        , [InfixR binOp]
        ]
        <??> "expression"

{- | A top level element, comprised of either an expression or a let declaration
Note that top level let declarations are not parsed here, but in the "Elara.Parse.Declaration" module
-}
element :: HParser Expr
element =
    (exprParser <|> statement) <??> "element"

{- | This is not a "statement" in an imperative sense, but is used to parse let expressions that are not at the top level.
| The reason for having this distinction is that if let's were considered a normal expression, then something like
| @let x = let y = 1@ would be considered valid code, which it is not. Having a separate parser means local let bindings can only be allowed
| in places where they are valid, such as in the body of a function.
-}
statement :: HParser Expr
statement = letExpression <??> "statement"

-- Lift a binary operator to work on `Expr` instead of `Frontend.Expr`. Probably not the best way to do this, but it works
liftedBinary :: Monad m => m t -> (t -> Expr -> Expr -> Frontend.Expr') -> m (Expr -> Expr -> Expr)
liftedBinary op f = do
    op' <- op
    let create l'@(Expr l) r'@(Expr r) =
            let region = enclosingRegion (getLocation l) (getLocation r)
             in Expr $ Located region (f op' l' r')
    pure create

binOp, functionCall :: HParser (Expr -> Expr -> Expr)
binOp = liftedBinary operator Frontend.BinaryOperator
functionCall = liftedBinary sc (const Frontend.FunctionCall)

-- This isn't actually used in `expressionTerm` as `varName` also covers (+) operators, but this is used when parsing infix applications
operator :: HParser Frontend.BinaryOperator
operator = Frontend.MkBinaryOperator <$> (asciiOp <|> infixOp) <??> "operator"
  where
    asciiOp = located $ do
        Frontend.Op <$> located (maybeQualified opName)
    infixOp = located $ lexeme $ do
        char' '`'
        endHead
        op <- located varName
        char' '`'
        pure $ Frontend.Infixed op

expression :: HParser Frontend.Expr
expression =
    unit
        <|> (parensExpr <??> "parenthesized expression")
        <|> (ifElse <??> "if expression")
        <|> (letInExpression <??> "let-in expression")
        <|> (lambda <??> "lambda expression")
        <|> (float <??> "float")
        <|> (int <??> "int")
        <|> (charL <??> "char")
        <|> (string <??> "string")
        <|> (variable <??> "variable")
        <|> (constructor <??> "constructor")
        <|> (list <??> "list")
        <??> "expression"

-- | Reserved words, used to backtrack accordingly
reservedWords :: Set Text
reservedWords = Set.fromList ["if", "else", "then", "let", "in", "class"]

parensExpr :: HParser Frontend.Expr
parensExpr = do
    e@(Frontend.Expr le) <- inParens exprParser
    pure (Frontend.Expr (Frontend.InParens e <$ le))

variable :: HParser Frontend.Expr
variable =
    locatedExpr $
        Frontend.Var <$> withPredicate (not . validName) (KeywordUsedAsName . nameText) (located $ lexeme varName)
  where
    validName var = nameText var `Set.member` reservedWords

constructor :: HParser Frontend.Expr
constructor = locatedExpr $ do
    con <- located $ lexeme (failIfDotAfter typeName)
    pure $ Frontend.Constructor con
  where
    -- Nasty hacky function that causes the parser to fail if the next token is a dot
    -- This is needed for cases like @a Prelude.+ b@. Without this function, it will parse as @(a Prelude) + (b)@, since module names and type names look the same.
    -- Having this parser fail means that the exprParser for function calls can't extend too far if there's a dot, causing it to get parsed as the intended @(Prelude.+) a b@
    failIfDotAfter :: HParser a -> HParser a
    failIfDotAfter p = do
        res <- p
        endHead
        o <- optional (char' '.')
        whenJust o $ \_ -> fail "Cannot use dot after expression"
        pure res

unit :: HParser Frontend.Expr
unit = locatedExpr (Frontend.Unit <$ symbol "()") <??> "unit"

int :: HParser Frontend.Expr
int = locatedExpr (Frontend.Int <$> integerLiteral) <??> "int"

float :: HParser Frontend.Expr
float = locatedExpr (Frontend.Float <$> floatLiteral) <??> "float"

string :: HParser Frontend.Expr
string = locatedExpr (Frontend.String <$> stringLiteral) <??> "string"

charL :: HParser Frontend.Expr
charL = locatedExpr (Frontend.Char <$> charLiteral) <??> "char"

list :: HParser Frontend.Expr
list = locatedExpr $ do
    symbol "["
    endHead
    elements <- lexeme (sepEndBy exprParser (symbol ","))
    symbol "]"
    pure $ Frontend.List elements

lambda :: HParser Expr
lambda = locatedExpr $ do
    symbol "\\"
    endHead
    start <- fromParsec indentLevel
    args <- lexeme (sepBy (lexeme pattern') sc)
    symbol "->"
    (_, res) <- blockAt start element
    pure (Frontend.Lambda args res)

ifElse :: HParser Expr
ifElse = locatedExpr $ do
    start <- sub1 <$> fromParsec indentLevel
    symbol "if"
    endHead
    (_, condition) <- blockAt start element
    _ <- withIndentOrNormal start (symbol "then")
    (_, thenBranch) <- blockAt start element
    _ <- withIndentOrNormal start (symbol "else")
    (_, elseBranch) <- blockAt start element

    pure (Frontend.If condition thenBranch elseBranch)

letExpression :: HParser Expr -- TODO merge this, Declaration.valueDecl, and letInExpression into 1 tidier thing
letExpression = locatedExpr $ do
    (_, name, patterns, e) <- letPreamble

    -- let names = patterns >>= patternNames
    -- let promote = fmap (transform (Name.promoteArguments names))
    pure (Frontend.Let name patterns e)

letInExpression :: HParser Frontend.Expr -- TODO merge this, Declaration.valueDecl, and letInExpression into 1 tidier thing
letInExpression = locatedExpr $ do
    (start, name, patterns, e) <- letPreamble

    _ <- withIndentOrNormal start (symbol "in")

    (_, body) <- blockAt start element

    -- let names = patterns >>= patternNames
    -- let promote = fmap (transform (Name.promoteArguments names))
    pure (Frontend.LetIn name patterns e body)

letPreamble :: HParser (Pos, Located (Unqualified VarName), [Frontend.Pattern], Expr)
letPreamble = do
    start <- fromParsec indentLevel
    symbol "let"
    H.endHead
    afterLet <- fromParsec indentLevel
    name <- lexeme $ located unqualifiedVarName
    (_, patterns) <- withCurrentIndentOrNormal $ do
        sepBy (lexeme pattern') sc
    _ <- withIndentOrNormal afterLet (symbol "=")
    (_, e) <- blockAt afterLet element
    pure (start, name, patterns, e)

letRaw :: HParser (Located (MaybeQualified VarName), [Frontend.Pattern], Frontend.Expr)
letRaw = do
    ((name, patterns), e) <- optionallyIndented letPreamble element
    pure (name, patterns, e)
  where
    letPreamble = do
        symbol "let"
        endHead
        name <- located $ lexeme varName
        patterns <- sepBy (lexeme pattern') sc
        symbol "="
        pure (name, patterns)
