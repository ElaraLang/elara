module Parse.Token where

data TokPosition = TokPosition
  { line :: Int,
    column :: Int
  }
  deriving (Eq, Show)

data Token
  = Let
  | Def
  | In
  | Eq
  | If
  | Then
  | Else
  | Match
  | Arrow
  | Backtick
  | NewLine
  | SemiColon
  | Colon
  | DoubleColon
  | Wildcard
  | Indent
  | Dedent
  | LParen
  | RParen
  | LSParen
  | RSParen
  | Comma
  | Int Integer
  | Str String
  | Identifier String
  | Operator String
  | TypeIdentifier String
  | EOF
  deriving (Show, Eq)