module AST.Source (Expr, Expr(..)) where

  data Expr
    = StringE String
    | IntE Integer
    | FloatE Double
    | CharE Char
    | VarE String Expr
    deriving (Show, Eq)

