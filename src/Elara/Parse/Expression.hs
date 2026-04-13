{-# LANGUAGE DataKinds #-}

module Elara.Parse.Expression (
    ExpressionGrammar (..),
    locatedExpr,
    letPreamble,
    expression,
    element,
    reservedWords,
)
where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Set qualified as Set
import Elara.AST.Extensions
import Elara.AST.Name (MaybeQualified (..), Name (..), VarName (..), nameText)
import Elara.AST.Phase (NoExtension (..))
import Elara.AST.Phases.Frontend
import Elara.AST.Region (Located (..), SourceRegion, enclosingRegion', sourceRegion, spanningRegion', withLocationOf)
import Elara.AST.Types
import Elara.Data.AtLeast2List qualified as AtLeast2List
import Elara.Lexer.Token (Token (..))
import Elara.Parse.Combinators (sepEndBy1')
import Elara.Parse.Error
import Elara.Parse.Indents
import Elara.Parse.Literal (charLiteral, floatLiteral, integerLiteral, stringLiteral, unitLiteral)
import Elara.Parse.Names (conName, opId, opName, unqualifiedVarName, varId, varName, varOrConName)
import Elara.Parse.Pattern
import Elara.Parse.Primitives (Parser, located, token_, withPredicate)
import Elara.Prim qualified as Prim
import Text.Megaparsec (MonadParsec (eof), choice, customFailure, sepEndBy, try, (<?>))
import Prelude hiding (Op)

-- | Data type putting all expression parsers together to avoid deadlocks in CAF initialisation.
data ExpressionGrammar = ExpressionGrammar
    { pElement :: Parser FrontendExpr
    , pExpression :: Parser FrontendExpr
    -- ^ the main expression parser
    }

-- | Helper to get the SourceRegion from a new-style Expr
exprRegion :: FrontendExpr -> SourceRegion
exprRegion (Expr loc _ _) = loc

locatedExpr :: Parser FrontendExpr' -> Parser FrontendExpr
locatedExpr p = do
    Located loc node <- located p
    pure $ Expr loc () node

{- | The main recursive expression parser.
Adds function application, cons, and binary operators on top of the basic 'term's.
-}
expression :: ExpressionGrammar -> Parser FrontendExpr
expression grammar =
    makeExprParser
        (term grammar)
        [ [InfixL functionCall]
        , [InfixL cons]
        , [InfixR binOp]
        ]
        <?> "expression"

-- | Parser of terms, i.e. expressions that don't require left recursion.
term :: ExpressionGrammar -> Parser FrontendExpr
term grammar =
    choice
        [ try unit
        , parensOrTuple grammar <?> "parenthesized or tuple expression"
        , list grammar <?> "list"
        , ifElse grammar <?> "if expression"
        , letInExpression grammar <?> "let-in expression"
        , lambda grammar <?> "lambda expression"
        , match grammar <?> "match expression"
        , float <?> "float"
        , int <?> "int"
        , charL <?> "char"
        , string <?> "string"
        , variable <?> "variable"
        , constructor <?> "constructor"
        ]
        <?> "expression"

{- | A top level element, comprised of either an expression or a let declaration
Note that top level let declarations are not parsed here, but in the "Elara.Parse.Declaration" module.
-}
element :: ExpressionGrammar -> Parser FrontendExpr
element grammar = choice [try (pExpression grammar), statement grammar]

{- | This is not a "statement" in an imperative sense, but is used to parse let expressions that are not at the top level.
The reason for having this distinction is that if let's were considered a normal expression, then something like
@let x = let y = 1@ would be considered valid code, which it is not. Having a separate parser means local let bindings can only be allowed
in places where they are valid, such as in the body of a function.
-}
statement :: ExpressionGrammar -> Parser FrontendExpr
statement grammar =
    letStatement grammar <?> "let statement"

binOp, cons, functionCall :: Parser (FrontendExpr -> FrontendExpr -> FrontendExpr)

-- | Parser for binary operators like (+), (-), (&&), etc.
binOp = infixOp operator (\op l r -> EExtension (FrontendBinaryOperator (BinaryOperatorExpression op l r)))

-- | Parser for the list constructor (::)
cons = infixOp consName (\op l r -> EExtension (FrontendBinaryOperator (BinaryOperatorExpression op l r)))
  where
    consName :: Parser FrontendBinaryOperator
    consName = do
        l <- located (token_ TokenDoubleColon)
        let Located loc _ = l
        let consValue = MaybeQualified (NameType "Cons") (Just Prim.primModuleName)
        pure $ InfixedOp loc (consValue `withLocationOf` l)

-- | Parser for function application (treated as an infix operator with no operator)
functionCall = infixOp pass (\_ f x -> EApp NoExtension f x)

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
        let region = enclosingRegion' (exprRegion l) (exprRegion r)
         in Expr region () (mkExpr op l r)

{- | This isn't actually used in 'expressionTerm' as 'varName' also covers @(+)@ operators,
but this is used when parsing infix applications
-}
operator :: Parser FrontendBinaryOperator
operator = (asciiOp <|> infixOp) <?> "operator"
  where
    asciiOp :: Parser FrontendBinaryOperator
    asciiOp = do
        Located loc op <- located (located opName)
        pure $ SymOp loc op
    infixOp = do
        Located loc inner <- located $ do
            token_ TokenBacktick
            op <- located varOrConName
            token_ TokenBacktick
            pure op
        pure $ InfixedOp loc inner

-- | Reserved words, used to backtrack accordingly
reservedWords :: Set Text
reservedWords = Set.fromList ["if", "else", "then", "def", "let", "in", "class"]

{- | Parse a parenthesized expression or a tuple in a single pass.
We consume '(' once, parse the first expression, then branch on ',' or ')'.
This allows us to avoid exponential backtracking when parsing nested tuples
-}
parensOrTuple :: ExpressionGrammar -> Parser FrontendExpr
parensOrTuple grammar = do
    Located loc res <- located $ do
        token_ TokenLeftParen
        first <- grammar.pExpression <?> "expression in parentheses or tuple"
        optional (try (optional lineSeparator *> token_ TokenComma)) >>= \case
            Nothing -> do
                optional lineSeparator
                token_ TokenRightParen
                pure $ Left first
            Just () -> do
                rest <- sepEndBy1' grammar.pExpression (token_ TokenComma)
                token_ TokenRightParen <?> "')' to close tuple"
                pure $ Right (AtLeast2List.fromHeadAndTail first rest)
    pure $ case res of
        Left e -> Expr loc () (EExtension (FrontendInParens (InParensExpression e)))
        Right items -> Expr loc () (EExtension (FrontendTuple (TupleExpression items)))

variable :: Parser FrontendExpr
variable =
    locatedExpr $
        EVar NoExtension <$> withPredicate (not . validName) KeywordUsedAsName (located varName)
  where
    validName var = nameText var `Set.member` reservedWords

constructor :: Parser FrontendExpr
constructor = locatedExpr $ do
    con <- located (failIfDotAfter conName)
    pure $ ECon NoExtension con
  where
    failIfDotAfter :: Parser a -> Parser a
    failIfDotAfter p = do
        res <- p
        o <- optional (token_ TokenDot)
        whenJust o $ \_ -> fail "Cannot use dot after expression"
        pure res

unit :: Parser FrontendExpr
unit = locatedExpr (EUnit <$ unitLiteral) <?> "unit"

int :: Parser FrontendExpr
int = locatedExpr (EInt <$> integerLiteral) <?> "int"

float :: Parser FrontendExpr
float = locatedExpr (EFloat <$> floatLiteral) <?> "float"

string :: Parser FrontendExpr
string = locatedExpr (EString <$> stringLiteral) <?> "string"

charL :: Parser FrontendExpr
charL = locatedExpr (EChar <$> charLiteral) <?> "char"

match :: ExpressionGrammar -> Parser FrontendExpr
match grammar = locatedExpr $ do
    token_ TokenMatch
    expr <- exprBlock grammar.pElement <?> "expression after 'match'"
    token_ TokenWith <?> "'with' keyword after match expression"

    let emptyMatchBody =
            token_ TokenLeftBrace *> token_ TokenRightBrace $> []
    let normalMatchBody = toList <$> block identity one (matchCase <?> "match case")
    cases <-
        (try emptyMatchBody <?> "empty match body")
            <|> (normalMatchBody <?> "match body")
    pure $ EMatch expr cases
  where
    matchCase :: Parser (FrontendPattern, FrontendExpr)
    matchCase = do
        case' <- patParser
        token_ TokenRightArrow

        expr <- exprBlock (grammar.pElement <?> "match case expression")
        pure (case', expr)

lambda :: ExpressionGrammar -> Parser FrontendExpr
lambda grammar = locatedExpr $ do
    bsLoc <- located (token_ TokenBackslash)

    args <- located (many patParser)
    arrLoc <- located (token_ TokenRightArrow <?> "'->' after lambda parameters")

    let emptyLambdaLoc = spanningRegion' (args ^. sourceRegion :| [bsLoc ^. sourceRegion, arrLoc ^. sourceRegion])
    let failEmptyBody =
            eof
                *> customFailure
                    (EmptyLambda emptyLambdaLoc)

    res <- failEmptyBody <|> exprBlock grammar.pElement <?> "lambda body"
    let Located _ patterns = args
    pure $ EExtension (FrontendMultiLam patterns res)

ifElse :: ExpressionGrammar -> Parser FrontendExpr
ifElse grammar = locatedExpr $ do
    token_ TokenIf
    condition <- grammar.pExpression <?> "condition after 'if'"
    _ <- optional lineSeparator
    token_ TokenThen <?> "'then' keyword after condition"
    thenBranch <- exprBlock grammar.pElement <?> "expression after 'then'"
    _ <- optional lineSeparator
    token_ TokenElse <?> "'else' branch"
    elseBranch <- exprBlock grammar.pElement <?> "expression after 'else'"
    pure (EIf condition thenBranch elseBranch)

letPreamble :: ExpressionGrammar -> Parser (Located VarName, [FrontendPattern], FrontendExpr)
letPreamble grammar = do
    token_ TokenLet
    (name, patterns) <- try infixDef <|> prefixDef
    token_ TokenEquals

    e <- exprBlock grammar.pElement
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

letInExpression :: ExpressionGrammar -> Parser FrontendExpr
letInExpression grammar = locatedExpr $ do
    (name, patterns, e) <- letPreamble grammar
    optional lineSeparator
    token_ TokenIn <?> "'in' keyword after let binding"

    body <- exprBlock grammar.pElement <?> "expression after 'in'"
    pure $ case patterns of
        [] -> ELetIn NoExtension name e body
        _ -> EExtension (FrontendLetInWithPatterns name patterns e body)

letStatement :: ExpressionGrammar -> Parser FrontendExpr
letStatement grammar = locatedExpr $ do
    (name, patterns, e) <- letPreamble grammar
    pure $ case patterns of
        [] -> ELet NoExtension name e
        _ -> EExtension (FrontendLetWithPatterns name patterns e)

list :: ExpressionGrammar -> Parser FrontendExpr
list grammar = locatedExpr $ do
    token_ TokenLeftBracket

    elements <- sepEndBy grammar.pElement (token_ TokenComma)
    token_ TokenRightBracket <?> "']' to close list"
    pure $ EExtension (FrontendList (ListExpression elements))
