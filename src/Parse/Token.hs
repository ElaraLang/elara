module Parse.Token where

data TokPosition
  = TokPosition { line :: Int
                , column :: Int }
  deriving (Eq, Show)

data Token = Let
            | In
            | Eq
            | If
            | Then
            | Else
            | Backtick
            | NewLine
            | SemiColon
            | Colon
            | DoubleColon
            | Indent
            | Dedent
            | LSParen
            | RSParen
            | Comma
            | Int Integer
            | Str String
            | Identifier String
            | Operator String
            | EOF
            deriving (Show, Eq)
