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
  | Type
  | Arrow
  | BigArrow
  | Backtick
  | Backslash
  | NewLine
  | SemiColon
  | Colon
  | DoubleColon
  | Wildcard
  | Indent
  | Dedent
  | LParen
  | RParen
  | LBrace
  | RBrace
  | LSParen
  | RSParen
  | Comma
  | Pipe
  | Int Integer
  | Str String
  | Identifier String
  | Operator String
  | TypeIdentifier String
  | EOF
  deriving (Show, Eq)
