module Elara.Lexer.Token where

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
    | -- | Variable Identifiers
      TokenVariableIdentifier Text
    | TokenConstructorIdentifier Text
    | -- | Other
      TokenOperatorIdentifier Text
    | TokenUnderscore
    | TokenEOF
    deriving (Show, Eq, Ord)

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
    TokenVariableIdentifier i -> i
    TokenConstructorIdentifier i -> i
    TokenOperatorIdentifier i -> i
    TokenUnderscore -> "_"
    TokenEOF -> "<EOF>"

startsNewLayout :: Token -> Int -> Bool
startsNewLayout TokenEquals _ = True
startsNewLayout TokenWhere _ = True
startsNewLayout TokenOf _ = True
startsNewLayout TokenIn _ = True
startsNewLayout TokenMatch _ = True
startsNewLayout TokenWith _ = True
startsNewLayout TokenRightArrow _ = True
startsNewLayout _ _ = False

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
