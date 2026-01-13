{-# LANGUAGE DataKinds #-}

module Elara.Parse.Expression (exprParser, locatedExpr, letPreamble, element, reservedWords) where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Set qualified as Set
import Elara.AST.Frontend
import Elara.AST.Generic (BinaryOperator (..), BinaryOperator' (..), Expr (Expr), Expr' (..))
import Elara.AST.Name (VarName (..), nameText)
import Elara.AST.Region (Located (..), enclosingRegion', sourceRegion, spanningRegion', withLocationOf)
import Elara.AST.Select (LocatedAST (Frontend))
import Elara.Data.AtLeast2List qualified as AtLeast2List
import Elara.Lexer.Token (Token (..))
import Elara.Parse.Combinators (sepEndBy1')
import Elara.Parse.Error
import Elara.Parse.Indents
import Elara.Parse.Literal (charLiteral, floatLiteral, integerLiteral, stringLiteral, unitLiteral)
import Elara.Parse.Names (conName, opId, opName, unqualifiedVarName, varId, varName, varOrConName)
import Elara.Parse.Pattern
import Elara.Parse.Primitives (Parser, inParens, located, token_, withPredicate)
import Elara.Prim qualified as Prim
import Elara.Utils (curry3)
import Text.Megaparsec (MonadParsec (eof), choice, customFailure, sepEndBy, try, (<?>))
import Prelude hiding (Op)

locatedExpr :: Parser FrontendExpr' -> Parser FrontendExpr
locatedExpr = fmap (\x -> Expr (x, Nothing)) . located

exprParser :: Parser FrontendExpr
exprParser =
    makeExprParser
        expression
        [ [InfixL functionCall]
        , [InfixL cons]
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

binOp, cons, functionCall :: Parser (FrontendExpr -> FrontendExpr -> FrontendExpr)

-- | Parser for binary operators like (+), (-), (&&), etc.
binOp = infixOp operator (curry3 BinaryOperator)

-- | Parser for the list constructor (::)
cons = infixOp consName (curry3 BinaryOperator)
  where
    consName :: Parser FrontendBinaryOperator
    consName = do
        l <- located (token_ TokenDoubleColon)
        let y = Infixed (Prim.cons `withLocationOf` l) :: BinaryOperator' Frontend
        pure $ MkBinaryOperator (y `withLocationOf` l)

-- | Parser for function application (treated as an infix operator with no operator)
functionCall = infixOp pass (\_ f x -> FunctionCall f x)

-- | Creates an infix operator parser that parses the operator, merges the regions, and wraps the result in `Expr`
infixOp ::
    -- | Parser that parses the operator
    Parser op ->
    -- | Function that creates the expression given the operator and two expressions
    (op -> FrontendExpr -> FrontendExpr -> FrontendExpr') ->
    -- | The lifted parser
    Parser (FrontendExpr -> FrontendExpr -> FrontendExpr)
infixOp opParser mkExpr = do
    op <- opParser
    pure $ \l r ->
        let region = enclosingRegion' (l ^. sourceRegion) (r ^. sourceRegion)
         in Expr (Located region (mkExpr op l r), Nothing)

-- This isn't actually used in `expressionTerm` as `varName` also covers (+) operators, but this is used when parsing infix applications
operator :: Parser FrontendBinaryOperator
operator = MkBinaryOperator <$> (asciiOp <|> infixOp) <?> "operator"
  where
    asciiOp :: Parser (Located FrontendBinaryOperator')
    asciiOp = located $ SymOp <$> located opName
    infixOp = located $ do
        token_ TokenBacktick
        op <- located varOrConName
        token_ TokenBacktick
        pure $ Infixed op

expression :: Parser FrontendExpr
expression =
    choice
        [ try unit
        , try tuple <?> "tuple expression"
        , try parensExpr <?> "parenthesized expression"
        , list <?> "list"
        , ifElse <?> "if expression"
        , letInExpression <?> "let-in expression"
        , lambda <?> "lambda expression"
        , match <?> "match expression"
        , float <?> "float"
        , int <?> "int"
        , charL <?> "char"
        , string <?> "string"
        , variable <?> "variable"
        , constructor <?> "constructor"
        ]
        <?> "expression"

-- | Reserved words, used to backtrack accordingly
reservedWords :: Set Text
reservedWords = Set.fromList ["if", "else", "then", "def", "let", "in", "class"]

parensExpr :: Parser FrontendExpr
parensExpr = do
    Located loc e <- located (inParens exprParser)
    pure $ Expr (Located loc (InParens e), Nothing)

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

    let emptyMatchBody =
            token_ TokenLeftBrace *> token_ TokenRightBrace $> []
    let normalMatchBody = toList <$> block identity one (matchCase <?> "match case")
    cases <-
        (try emptyMatchBody <?> "empty match body") -- allow empty match blocks
            <|> (normalMatchBody <?> "match body")
    pure $ Match expr cases
  where
    matchCase :: Parser (FrontendPattern, FrontendExpr)
    matchCase = do
        case' <- patParser
        token_ TokenRightArrow

        expr <- exprBlock (element <?> "match case expression")
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
    _ <- optional lineSeparator
    token_ TokenThen
    thenBranch <- exprBlock element
    _ <- optional lineSeparator
    token_ TokenElse
    elseBranch <- exprBlock element
    pure (If condition thenBranch elseBranch)

letPreamble :: Parser (Located VarName, [FrontendPattern], FrontendExpr)
letPreamble = do
    token_ TokenLet
    (name, patterns) <- try infixDef <|> prefixDef
    token_ TokenEquals

    e <- exprBlock element
    pure (name, patterns, e)
  where
    prefixDef :: Parser (Located VarName, [FrontendPattern])
    prefixDef = do
        name <- located unqualifiedVarName
        patterns <- many patParser
        pure (name, patterns)

    infixDef :: Parser (Located VarName, [FrontendPattern])
    infixDef = do
        p1 <- patParser
        op <- located (opName' <|> backTickedName)
        p2 <- patParser
        pure (op, [p1, p2])
      where
        opName' = fmap OperatorVarName opId <?> "operator name"
        backTickedName =
            ( token_ TokenBacktick
                *> fmap NormalVarName varId
                <* token_ TokenBacktick
            )
                <?> "backticked name"

letInExpression :: Parser FrontendExpr -- TODO merge this, Declaration.valueDecl, and letInExpression into 1 tidier thing
letInExpression = locatedExpr $ do
    (name, patterns, e) <- letPreamble
    optional lineSeparator -- if the body is a nested block, there will be a line separator at the end
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
    pure $ Tuple (AtLeast2List.fromHeadAndTail firstElement otherElements)

list :: Parser FrontendExpr
list = locatedExpr $ do
    token_ TokenLeftBracket

    elements <- sepEndBy exprParser (token_ TokenComma)
    token_ TokenRightBracket
    pure $ List elements
