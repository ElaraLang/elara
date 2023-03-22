module Elara.Parse.Expression where

import Control.Lens ((^.))
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Set qualified as Set
import Elara.AST.Frontend (Expr (..), Pattern (..))
import Elara.AST.Frontend qualified as Frontend
import Elara.AST.Name (VarName, nameText)
import Elara.AST.Region (Located (..), enclosingRegion', sourceRegion)
import Elara.Lexer.Token (Token (..))
import Elara.Parse.Error
import Elara.Parse.Indents
import Elara.Parse.Literal (charLiteral, floatLiteral, integerLiteral, stringLiteral)
import Elara.Parse.Names (maybeQualified, opName, typeName, unqualifiedVarName, varName)
import Elara.Parse.Pattern
import Elara.Parse.Primitives (HParser, inBraces, inParens, located, token', withPredicate, (<??>))
import HeadedMegaparsec (endHead)
import HeadedMegaparsec qualified as H (parse, toParsec)
import Text.Megaparsec (sepEndBy, sepEndBy1)

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
statement = letStatement <??> "statement"

-- Lift a binary operator to work on `Expr` instead of `Frontend.Expr`. Probably not the best way to do this, but it works
liftedBinary :: Monad m => m t -> (t -> Expr -> Expr -> Frontend.Expr') -> m (Expr -> Expr -> Expr)
liftedBinary op f = do
    op' <- op
    let create l'@(Expr l) r'@(Expr r) =
            let region = enclosingRegion' (l ^. sourceRegion) (r ^. sourceRegion)
             in Expr $ Located region (f op' l' r')
    pure create

binOp, functionCall :: HParser (Expr -> Expr -> Expr)
binOp = liftedBinary operator Frontend.BinaryOperator
functionCall = liftedBinary pass (const Frontend.FunctionCall)

-- This isn't actually used in `expressionTerm` as `varName` also covers (+) operators, but this is used when parsing infix applications
operator :: HParser Frontend.BinaryOperator
operator = Frontend.MkBinaryOperator <$> (asciiOp <|> infixOp) <??> "operator"
  where
    asciiOp = located $ do
        Frontend.Op <$> located (maybeQualified opName)
    infixOp = located $ do
        token' TokenBacktick
        endHead
        op <- located varName
        token' TokenBacktick
        pure $ Frontend.Infixed op

expression :: HParser Frontend.Expr
expression =
    unit
        <|> (parensExpr <??> "parenthesized expression")
        <|> (ifElse <??> "if expression")
        <|> (letInExpression <??> "let-in expression")
        <|> (lambda <??> "lambda expression")
        <|> (match <??> "match expression")
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
        Frontend.Var <$> withPredicate (not . validName) KeywordUsedAsName (located varName)
  where
    validName var = nameText var `Set.member` reservedWords

constructor :: HParser Frontend.Expr
constructor = locatedExpr $ do
    con <- located (failIfDotAfter typeName)
    pure $ Frontend.Constructor con
  where
    -- Nasty hacky function that causes the parser to fail if the next token is a dot
    -- This is needed for cases like @a Prelude.+ b@. Without this function, it will parse as @(a Prelude) + (b)@, since module names and type names look the same.
    -- Having this parser fail means that the exprParser for function calls can't extend too far if there's a dot, causing it to get parsed as the intended @(Prelude.+) a b@
    failIfDotAfter :: HParser a -> HParser a
    failIfDotAfter p = do
        res <- p
        endHead
        o <- optional (token' TokenDot)
        whenJust o $ \_ -> fail "Cannot use dot after expression"
        pure res

unit :: HParser Frontend.Expr
unit = locatedExpr (Frontend.Unit <$ token' TokenLeftParen <* token' TokenRightParen) <??> "unit"

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
    token' TokenLeftBracket
    endHead
    elements <- sepEndBy exprParser (token' TokenComma)
    token' TokenRightBracket
    pure $ Frontend.List elements

match :: HParser Frontend.Expr
match = locatedExpr $ do
    token' TokenMatch
    endHead
    expr <- block element
    token' TokenWith

    cases <- inBraces $ sepEndBy1 matchCase (token' TokenSemicolon)
    pure $ Frontend.Match expr cases
  where
    matchCase :: HParser (Pattern, Frontend.Expr)
    matchCase = do
        case' <- pattern'
        token' TokenRightArrow
        expr <- block element
        pure (case', expr)

lambda :: HParser Expr
lambda = locatedExpr $ do
    token' TokenBackslash
    endHead
    args <- many pattern'
    token' TokenRightArrow
    res <- block element
    pure (Frontend.Lambda args res)

ifElse :: HParser Expr
ifElse = locatedExpr $ do
    token' TokenIf
    endHead
    condition <- exprParser
    _ <- optional (token' TokenSemicolon)
    token' TokenThen
    thenBranch <- block element
    _ <- optional (token' TokenSemicolon)
    token' TokenElse
    elseBranch <- block element
    pure (Frontend.If condition thenBranch elseBranch)

letInExpression :: HParser Frontend.Expr -- TODO merge this, Declaration.valueDecl, and letInExpression into 1 tidier thing
letInExpression = locatedExpr $ do
    (name, patterns, e) <- letPreamble
    token' TokenIn
    body <- block element

    -- let names = patterns >>= patternNames
    -- let promote = fmap (transform (Name.promoteArguments names))
    pure (Frontend.LetIn name patterns e body)

letStatement :: HParser Frontend.Expr
letStatement = locatedExpr $ do
    (name, patterns, e) <- letPreamble
    pure (Frontend.Let name patterns e)

letPreamble :: HParser (Located VarName, [Frontend.Pattern], Frontend.Expr)
letPreamble = do
    token' TokenLet
    endHead
    name <- located unqualifiedVarName
    patterns <- many pattern'
    token' TokenEquals
    e <- block element
    pure (name, patterns, e)
