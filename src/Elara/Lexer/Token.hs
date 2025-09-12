{-# LANGUAGE StrictData #-}

module Elara.Lexer.Token where

import Data.Set qualified as Set
import Elara.AST.Name (ModuleName, nameText)
import Elara.AST.Region (Located, RealPosition)
import Elara.Data.Pretty

type Lexeme = Located Token

type TokPosition = RealPosition

data Token
    = -- | ;
      TokenSemicolon
    | -- | ,
      TokenComma
    | -- | .
      TokenDot
    | -- | :
      TokenColon
    | -- | ::
      TokenDoubleColon
    | -- | =
      TokenEquals
    | -- | \
      TokenBackslash
    | -- | |
      TokenPipe
    | -- | (
      TokenLeftParen
    | -- | )
      TokenRightParen
    | -- | {
      TokenLeftBrace
    | -- | }
      TokenRightBrace
    | -- | [
      TokenLeftBracket
    | -- | ]
      TokenRightBracket
    | -- | <-
      TokenLeftArrow
    | -- | ->
      TokenRightArrow
    | -- | =>
      TokenDoubleRightArrow
    | -- | @
      TokenAt
    | -- | `
      TokenBacktick
    | TokenInt Integer
    | TokenFloat Double
    | TokenChar Char
    | -- | Keywords
      TokenString Text
    | TokenLet
    | TokenDef
    | TokenIn
    | TokenIf
    | TokenThen
    | TokenElse
    | TokenCase
    | TokenOf
    | TokenData
    | TokenType
    | TokenAlias
    | TokenModule
    | TokenImport
    | TokenAs
    | TokenQualified
    | TokenExposing
    | TokenWhere
    | TokenForall
    | TokenClass
    | TokenInstance
    | TokenDeriving
    | TokenMatch
    | TokenWith
    | TokenInfixL
    | TokenInfixR
    | TokenInfix
    | -- | Variable Identifiers
      TokenVariableIdentifier Text
    | TokenConstructorIdentifier Text
    | -- | Other
      TokenOperatorIdentifier Text
    | TokenQVariableIdentifier (ModuleName, Text)
    | TokenQConstructorIdentifier (ModuleName, Text)
    | TokenQOperatorIdentifier (ModuleName, Text)
    | TokenUnderscore
    | TokenIndent
    | TokenDedent
    | TokenLineSeparator
    | TokenEOF
    deriving (Show, Eq, Ord, Generic)
instance Hashable Token

tokenRepr :: Token -> Text
tokenRepr = \case
    TokenSemicolon -> ";"
    TokenComma -> ","
    TokenDot -> "."
    TokenColon -> ":"
    TokenDoubleColon -> "::"
    TokenEquals -> "="
    TokenBackslash -> "\\"
    TokenPipe -> "|"
    TokenLeftParen -> "("
    TokenRightParen -> ")"
    TokenLeftBrace -> "{"
    TokenRightBrace -> "}"
    TokenLeftBracket -> "["
    TokenRightBracket -> "]"
    TokenLeftArrow -> "<-"
    TokenRightArrow -> "->"
    TokenDoubleRightArrow -> "=>"
    TokenAt -> "@"
    TokenBacktick -> "`"
    TokenInt i -> show i
    TokenFloat f -> show f
    TokenChar c -> show c
    TokenString s -> show s
    TokenLet -> "let"
    TokenDef -> "def"
    TokenIn -> "in"
    TokenIf -> "if"
    TokenThen -> "then"
    TokenElse -> "else"
    TokenCase -> "case"
    TokenOf -> "of"
    TokenData -> "data"
    TokenType -> "type"
    TokenAlias -> "alias"
    TokenModule -> "module"
    TokenImport -> "import"
    TokenAs -> "as"
    TokenQualified -> "qualified"
    TokenExposing -> "exposing"
    TokenWhere -> "where"
    TokenForall -> "forall"
    TokenClass -> "class"
    TokenInstance -> "instance"
    TokenDeriving -> "deriving"
    TokenMatch -> "match"
    TokenWith -> "with"
    TokenInfixL -> "infixl"
    TokenInfixR -> "infixr"
    TokenInfix -> "infix"
    TokenVariableIdentifier i -> i
    TokenConstructorIdentifier i -> i
    TokenOperatorIdentifier i -> i
    TokenQVariableIdentifier (m, i) -> nameText m <> "." <> i
    TokenQConstructorIdentifier (m, i) -> nameText m <> "." <> i
    TokenQOperatorIdentifier (m, i) -> nameText m <> "." <> i
    TokenUnderscore -> "_"
    TokenIndent -> "<INDENT>"
    TokenDedent -> "<DEDENT>"
    TokenLineSeparator -> "<LINESEP>"
    TokenEOF -> "<EOF>"

unsafeTokenText :: Token -> Text
unsafeTokenText = \case
    TokenVariableIdentifier i -> i
    TokenConstructorIdentifier i -> i
    TokenOperatorIdentifier i -> i
    t -> error ("unsafeTokenText: " <> show t)

unsafeTokenInt :: Token -> Integer
unsafeTokenInt = \case
    TokenInt i -> i
    t -> error ("unsafeTokenInt: " <> show t)

unsafeTokenFloat :: Token -> Double
unsafeTokenFloat = \case
    TokenFloat f -> f
    t -> error ("unsafeTokenFloat: " <> show t)

isIndent :: Token -> Bool
isIndent TokenIndent = True
isIndent TokenDedent = True
isIndent TokenLineSeparator = True
isIndent _ = False

instance Pretty Token where
    pretty = pretty . tokenRepr

{- | Returns 'True' if the token ends an expression.
This is used to determine whether line breaks should be treated as semicolons
If the token doesn't end an expression, then a line break is skipped.
If the token does end an expression, then a line break is treated as a semicolon.
-}
tokenEndsExpr :: Token -> Bool
tokenEndsExpr = \case
    -- tokens that DO NOT end an expression (expect a continuation)
    TokenOperatorIdentifier _ -> False
    TokenQOperatorIdentifier _ -> False
    TokenBackslash -> False
    TokenLeftParen -> False
    TokenLeftBracket -> False
    TokenLeftBrace -> False
    TokenComma -> False
    TokenEquals -> False
    TokenColon -> False
    TokenDoubleColon -> False
    TokenDot -> False
    TokenIndent -> False
    -- important: decent does end an expression because it closes a block
    TokenDedent -> True
    -- everything else ends an expression:
    -- identifiers (var/ctor/qualified), literals, keywords, closers, LINESEP, EOF, etc.
    _ -> True
