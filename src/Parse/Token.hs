module Parse.Token where

data TokPosition
  = TokPosition { line :: Int
                , column :: Int }
  deriving (Eq, Show)

data Token = Let
            | In
            | Eq
            | If
            | Else
            | Backtick
            | NewLine
            | SemiColon
            | Indent
            | Dedent
            | LSParen
            | RSParen
            | Comma
            | Int Int
            | Str String
            | Identifier String
            | Operator String
            | EOF
            deriving (Show, Eq)
