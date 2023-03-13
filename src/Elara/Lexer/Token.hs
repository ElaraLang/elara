module Elara.Lexer.Token where

data Token
  = -- | Symbols
    TokenSemicolon
  | -- | ;
    TokenComma
  | -- | ,
    TokenDot
  | -- | .
    TokenColon
  | -- | :
    TokenDoubleColon
  | -- | ::
    TokenEquals
  | -- | =
    TokenBackslash
  | -- | \
    TokenPipe
  | -- | |
    TokenLeftParen
  | -- | (
    TokenRightParen
  | -- | )
    TokenLeftBrace
  | -- | {
    TokenRightBrace
  | -- | }
    TokenLeftBracket
  | -- | [
    TokenRightBracket
  | -- | ]
    TokenLeftArrow
  | -- | <-
    TokenRightArrow
  | -- | ->
    TokenDoubleRightArrow
  | -- | =>
    TokenAt
  | -- \| Literals

    -- | @
    TokenInt Integer
  | TokenFloat Double
  | TokenChar Char
  | TokenString Text
  | -- | Keywords
    TokenLet
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
  | TokenWhere
  | TokenForall
  | TokenClass
  | TokenInstance
  | TokenDeriving
  | TokenMatch
  | -- | Identifiers
    TokenVariableIdentifier Text
  | TokenConstructorIdentifier Text
  | -- | Other
    TokenUnderscore
  | TokenEOF
  deriving (Show, Eq)

startsNewLayout :: Token -> Int -> Bool
startsNewLayout TokenEquals _ = True
startsNewLayout TokenWhere _ = True
startsNewLayout TokenOf _ = True
startsNewLayout TokenIn _ = True
startsNewLayout _ 3 = True
startsNewLayout _ _ = False