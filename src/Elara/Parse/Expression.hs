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
import Elara.Parse.Primitives (HParser, IsParser (fromParsec), inParens, located, token_, withPredicate, (<??>))
import Elara.Utils (curry3)
import HeadedMegaparsec (endHead, wrapToHead)
import HeadedMegaparsec qualified as H (parse, toParsec)
import Text.Megaparsec (MonadParsec (eof), customFailure, sepEndBy)
import Prelude hiding (Op)

locatedExpr :: HParser FrontendExpr' -> HParser FrontendExpr
locatedExpr = fmap (\x -> Expr (x, Nothing)) . (H.parse . located . H.toParsec)

exprParser :: HParser FrontendExpr
exprParser =
  makeExprParser
    expression
    [ [InfixL functionCall],
      [InfixR binOp]
    ]
    <??> "expression"

-- | A top level element, comprised of either an expression or a let declaration
-- Note that top level let declarations are not parsed here, but in the "Elara.Parse.Declaration" module
element :: HParser FrontendExpr
element =
  (exprParser <|> statement) <??> "element"

-- | This is not a "statement" in an imperative sense, but is used to parse let expressions that are not at the top level.
-- | The reason for having this distinction is that if let's were considered a normal expression, then something like
-- | @let x = let y = 1@ would be considered valid code, which it is not. Having a separate parser means local let bindings can only be allowed
-- | in places where they are valid, such as in the body of a function.
statement :: HParser FrontendExpr
statement =
  letStatement <??> "let statement"

unannotatedExpr :: Iso' FrontendExpr (Located FrontendExpr')
unannotatedExpr = iso (\(Expr (e, _)) -> e) (\x -> Expr (x, Nothing))

binOp, functionCall :: HParser (FrontendExpr -> FrontendExpr -> FrontendExpr)
binOp = liftedBinary operator (curry3 BinaryOperator) unannotatedExpr
functionCall = liftedBinary pass (const FunctionCall) unannotatedExpr

-- This isn't actually used in `expressionTerm` as `varName` also covers (+) operators, but this is used when parsing infix applications
operator :: HParser FrontendBinaryOperator
operator = MkBinaryOperator <$> (asciiOp <|> infixOp) <??> "operator"
  where
    asciiOp :: HParser (Located FrontendBinaryOperator')
    asciiOp = located $ do
      SymOp <$> located opName
    infixOp = located $ do
      token_ TokenBacktick
      endHead
      op <- located varOrConName
      token_ TokenBacktick
      pure $ Infixed op

expression :: HParser FrontendExpr
expression =
  unit
    <|> (tuple <??> "tuple expression")
    <|> (parensExpr <??> "parenthesized expression")
    <|> (list <??> "list")
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
      <??> "expression"

-- | Reserved words, used to backtrack accordingly
reservedWords :: Set Text
reservedWords = Set.fromList ["if", "else", "then", "def", "let", "in", "class"]

parensExpr :: HParser FrontendExpr
parensExpr = do
  e@(Expr (le, t)) <- inParens exprParser
  pure (Expr (InParens e <$ le, t))

variable :: HParser FrontendExpr
variable =
  locatedExpr $
    Var <$> withPredicate (not . validName) KeywordUsedAsName (located varName)
  where
    validName var = nameText var `Set.member` reservedWords

constructor :: HParser FrontendExpr
constructor = locatedExpr $ do
  con <- located (failIfDotAfter conName)
  pure $ Constructor con
  where
    -- Nasty hacky function that causes the parser to fail if the next token is a dot
    -- This is needed for cases like @a Prelude.+ b@. Without this function, it will parse as @(a Prelude) + (b)@, since module names and type names look the same.
    -- Having this parser fail means that the exprParser for function calls can't extend too far if there's a dot, causing it to get parsed as the intended @(Prelude.+) a b@
    failIfDotAfter :: HParser a -> HParser a
    failIfDotAfter p = do
      res <- p
      o <- optional (token_ TokenDot)
      whenJust o $ \_ -> fail "Cannot use dot after expression"
      pure res

unit :: HParser FrontendExpr
unit = locatedExpr (Unit <$ unitLiteral) <??> "unit"

int :: HParser FrontendExpr
int = locatedExpr (Int <$> integerLiteral) <??> "int"

float :: HParser FrontendExpr
float = locatedExpr (Float <$> floatLiteral) <??> "float"

string :: HParser FrontendExpr
string = locatedExpr (String <$> stringLiteral) <??> "string"

charL :: HParser FrontendExpr
charL = locatedExpr (Char <$> charLiteral) <??> "char"

match :: HParser FrontendExpr
match = wrapToHead $ locatedExpr $ do
  token_ TokenMatch
  endHead
  expr <- exprBlock element
  token_ TokenWith

  cases <-
    (toList <$> block identity pure matchCase)
      <|> (token_ TokenLeftBrace *> token_ TokenRightBrace $> []) -- allow empty match blocks
  pure $ Match expr cases
  where
    matchCase :: HParser (FrontendPattern, FrontendExpr)
    matchCase = do
      case' <- patParser
      token_ TokenRightArrow
      endHead
      expr <- exprBlock element
      pure (case', expr)

lambda :: HParser FrontendExpr
lambda = locatedExpr $ do
  bsLoc <- located (token_ TokenBackslash)
  endHead
  args <- located (many patParser)
  arrLoc <- located (token_ TokenRightArrow)

  let emptyLambdaLoc = spanningRegion' (args ^. sourceRegion :| [bsLoc ^. sourceRegion, arrLoc ^. sourceRegion])
  let failEmptyBody =
        fromParsec
          ( eof
              *> customFailure
                (EmptyLambda emptyLambdaLoc)
          )
  res <- failEmptyBody <|> exprBlock element
  pure (Lambda args res)

ifElse :: HParser FrontendExpr
ifElse = locatedExpr $ do
  token_ TokenIf
  endHead
  condition <- exprParser
  _ <- optional (token_ TokenSemicolon)
  token_ TokenThen
  thenBranch <- exprBlock element
  _ <- optional (token_ TokenSemicolon)
  token_ TokenElse
  elseBranch <- exprBlock element
  pure (If condition thenBranch elseBranch)

letInExpression :: HParser FrontendExpr -- TODO merge this, Declaration.valueDecl, and letInExpression into 1 tidier thing
letInExpression = locatedExpr $ do
  (name, patterns, e) <- letPreamble
  token_ TokenIn
  endHead
  body <- exprBlock element

  -- let names = patterns >>= patternNames
  -- let promote = fmap (transform (Name.promoteArguments names))
  pure (LetIn name patterns e body)

letStatement :: HParser FrontendExpr
letStatement = locatedExpr $ do
  (name, patterns, e) <- letPreamble
  pure (Let name patterns e)

letPreamble :: HParser (Located VarName, [FrontendPattern], FrontendExpr)
letPreamble = do
  token_ TokenLet
  endHead
  name <- located unqualifiedVarName
  patterns <- many patParser
  token_ TokenEquals
  e <- exprBlock element
  pure (name, patterns, e)

tuple :: HParser FrontendExpr
tuple = locatedExpr $ do
  token_ TokenLeftParen
  endHead
  firstElement <- exprParser
  token_ TokenComma
  otherElements <- sepEndBy1' exprParser (token_ TokenComma)
  token_ TokenRightParen
  pure $ Tuple (firstElement `NE.cons` otherElements)

list :: HParser FrontendExpr
list = locatedExpr $ do
  token_ TokenLeftBracket
  endHead
  elements <- sepEndBy exprParser (token_ TokenComma)
  token_ TokenRightBracket
  pure $ List elements
