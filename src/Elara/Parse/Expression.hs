module Elara.Parse.Expression where

import Control.Lens (Iso', iso, (^.))
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Elara.AST.Frontend
import Elara.AST.Generic (BinaryOperator (..), BinaryOperator' (..), Expr (Expr), Expr' (..))
import Elara.AST.Name (VarName, nameText)
import Elara.AST.Region (Located (..), sourceRegion, spanningRegion')
import Elara.Lexer.Token (Token (..))
import Elara.Parse.Combinators (liftedBinary, sepEndBy1')
import Elara.Parse.Error
import Elara.Parse.Indents
import Elara.Parse.Literal (charLiteral, floatLiteral, integerLiteral, stringLiteral, unitLiteral)
import Elara.Parse.Names (conName, opName, unqualifiedVarName, varName, varOrConName)
import Elara.Parse.Pattern
import Elara.Parse.Primitives (Parser, inParens, located, token_, withPredicate)
import Elara.Utils (curry3)
import Text.Megaparsec (MonadParsec (eof), customFailure, sepEndBy, try, (<?>))
import Prelude hiding (Op)

locatedExpr :: Parser FrontendExpr' -> Parser FrontendExpr
locatedExpr = fmap (\x -> Expr (x, Nothing)) . located

exprParser :: Parser FrontendExpr
exprParser =
    makeExprParser
        expression
        [ [InfixL functionCall]
        , [InfixR binOp]
        ]
        <?> "expression"

{- | A top level element, comprised of either an expression or a let declaration
Note that top level let declarations are not parsed here, but in the "Elara.Parse.Declaration" module
-}
element :: Parser FrontendExpr
element =
    (try exprParser <|> statement) <?> "element"

{- | This is not a "statement" in an imperative sense, but is used to parse let expressions that are not at the top level.
| The reason for having this distinction is that if let's were considered a normal expression, then something like
| @let x = let y = 1@ would be considered valid code, which it is not. Having a separate parser means local let bindings can only be allowed
| in places where they are valid, such as in the body of a function.
-}
statement :: Parser FrontendExpr
statement =
    letStatement <?> "let statement"

unannotatedExpr :: Iso' FrontendExpr (Located FrontendExpr')
unannotatedExpr = iso (\(Expr (e, _)) -> e) (\x -> Expr (x, Nothing))

binOp, functionCall :: Parser (FrontendExpr -> FrontendExpr -> FrontendExpr)
binOp = liftedBinary operator (curry3 BinaryOperator) unannotatedExpr
functionCall = liftedBinary pass (const FunctionCall) unannotatedExpr

-- This isn't actually used in `expressionTerm` as `varName` also covers (+) operators, but this is used when parsing infix applications
operator :: Parser FrontendBinaryOperator
operator = MkBinaryOperator <$> (asciiOp <|> infixOp) <?> "operator"
  where
    asciiOp :: Parser (Located FrontendBinaryOperator')
    asciiOp = located $ do
        SymOp <$> located opName
    infixOp = located $ do
        token_ TokenBacktick
        op <- located varOrConName
        token_ TokenBacktick
        pure $ Infixed op

expression :: Parser FrontendExpr
expression =
    try unit
        <|> (try tuple <?> "tuple expression")
        <|> (parensExpr <?> "parenthesized expression")
        <|> (list <?> "list")
        <|> (ifElse <?> "if expression")
        <|> (letInExpression <?> "let-in expression")
        <|> (lambda <?> "lambda expression")
        <|> (match <?> "match expression")
        <|> (float <?> "float")
        <|> (int <?> "int")
        <|> (charL <?> "char")
        <|> (string <?> "string")
        <|> (variable <?> "variable")
        <|> (constructor <?> "constructor")
        <?> "expression"

-- | Reserved words, used to backtrack accordingly
reservedWords :: Set Text
reservedWords = Set.fromList ["if", "else", "then", "def", "let", "in", "class"]

parensExpr :: Parser FrontendExpr
parensExpr = inParens exprParser

variable :: Parser FrontendExpr
variable =
    locatedExpr $
        Var <$> withPredicate (not . validName) KeywordUsedAsName (located varName)
  where
    validName var = nameText var `Set.member` reservedWords

constructor :: Parser FrontendExpr
constructor = locatedExpr $ do
    con <- located (failIfDotAfter conName)
    pure $ Constructor con
  where
    -- Nasty hacky function that causes the parser to fail if the next token is a dot
    -- This is needed for cases like @a Prelude.+ b@. Without this function, it will parse as @(a Prelude) + (b)@, since module names and type names look the same.
    -- Having this parser fail means that the exprParser for function calls can't extend too far if there's a dot, causing it to get parsed as the intended @(Prelude.+) a b@
    failIfDotAfter :: Parser a -> Parser a
    failIfDotAfter p = do
        res <- p
        o <- optional (token_ TokenDot)
        whenJust o $ \_ -> fail "Cannot use dot after expression"
        pure res

unit :: Parser FrontendExpr
unit = locatedExpr (Unit <$ unitLiteral) <?> "unit"

int :: Parser FrontendExpr
int = locatedExpr (Int <$> integerLiteral) <?> "int"

float :: Parser FrontendExpr
float = locatedExpr (Float <$> floatLiteral) <?> "float"

string :: Parser FrontendExpr
string = locatedExpr (String <$> stringLiteral) <?> "string"

charL :: Parser FrontendExpr
charL = locatedExpr (Char <$> charLiteral) <?> "char"

match :: Parser FrontendExpr
match = locatedExpr $ do
    token_ TokenMatch
    expr <- exprBlock element
    token_ TokenWith

    cases <-
        try (toList <$> block identity one matchCase)
            <|> (token_ TokenLeftBrace *> token_ TokenRightBrace $> []) -- allow empty match blocks
    pure $ Match expr cases
  where
    matchCase :: Parser (FrontendPattern, FrontendExpr)
    matchCase = do
        case' <- patParser
        token_ TokenRightArrow

        expr <- exprBlock element
        pure (case', expr)

lambda :: Parser FrontendExpr
lambda = locatedExpr $ do
    bsLoc <- located (token_ TokenBackslash)

    args <- located (many patParser)
    arrLoc <- located (token_ TokenRightArrow)

    let emptyLambdaLoc = spanningRegion' (args ^. sourceRegion :| [bsLoc ^. sourceRegion, arrLoc ^. sourceRegion])
    let failEmptyBody =
            eof
                *> customFailure
                    (EmptyLambda emptyLambdaLoc)

    res <- failEmptyBody <|> exprBlock element
    pure (Lambda args res)

ifElse :: Parser FrontendExpr
ifElse = locatedExpr $ do
    token_ TokenIf

    condition <- exprParser
    _ <- optional (token_ TokenSemicolon)
    token_ TokenThen
    thenBranch <- exprBlock element
    _ <- optional (token_ TokenSemicolon)
    token_ TokenElse
    elseBranch <- exprBlock element
    pure (If condition thenBranch elseBranch)

letPreamble :: Parser (Located VarName, [FrontendPattern], FrontendExpr)
letPreamble = do
    token_ TokenLet
    name <- located unqualifiedVarName
    patterns <- many patParser
    token_ TokenEquals

    e <- exprBlock element
    pure (name, patterns, e)

letInExpression :: Parser FrontendExpr -- TODO merge this, Declaration.valueDecl, and letInExpression into 1 tidier thing
letInExpression = locatedExpr $ do
    (name, patterns, e) <- letPreamble
    token_ TokenIn

    body <- exprBlock element
    pure (LetIn name patterns e body)

letStatement :: Parser FrontendExpr
letStatement = locatedExpr $ do
    (name, patterns, e) <- letPreamble
    pure (Let name patterns e)

tuple :: Parser FrontendExpr
tuple = locatedExpr $ do
    token_ TokenLeftParen
    firstElement <- exprParser
    token_ TokenComma
    otherElements <- sepEndBy1' exprParser (token_ TokenComma)
    token_ TokenRightParen
    pure $ Tuple (firstElement `NE.cons` otherElements)

list :: Parser FrontendExpr
list = locatedExpr $ do
    token_ TokenLeftBracket

    elements <- sepEndBy exprParser (token_ TokenComma)
    token_ TokenRightBracket
    pure $ List elements
