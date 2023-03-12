module Elara.Lexer.Token where

data Token
    = -- | Literals
      TokenSemicolon
    | TokenInt Integer
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
    | -- | Other
      TokenUnderscore
    | TokenEOF
    deriving (Show, Eq)