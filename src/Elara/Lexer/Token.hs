module Elara.Lexer.Token where

data Token
  = -- | Symbols
    TokenSemicolon
  | TokenComma
  | TokenDot
  | TokenColon
  | TokenDoubleColon
  | TokenEquals
  | TokenBackslash
  | TokenPipe
  | TokenLeftParen
  | TokenRightParen
  | TokenLeftBrace
  | TokenRightBrace
  | TokenLeftBracket
  | TokenRightBracket
  | TokenLeftArrow
  | TokenRightArrow
  | TokenDoubleRightArrow
  | TokenAt
  | -- | Literals
    TokenInt Integer
  | TokenFloat Text
  | TokenChar Text
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
  | -- | Identifiers
    TokenVariableIdentifier Text
  | TokenConstructorIdentifier Text
  | -- | Other
    TokenUnderscore
  | TokenEOF
  deriving (Show, Eq)

startsNewLayout :: Token -> Bool
startsNewLayout TokenEquals = True
startsNewLayout TokenWhere = True
startsNewLayout TokenOf = True
startsNewLayout _ = False
