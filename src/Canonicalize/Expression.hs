module Canonicalize.Expression where
import qualified AST.Source as Src
import qualified AST.Canonical as Can

canonicalize :: Src.Expr -> Can.Expr
canonicalize exp = do
    case exp of
        Src.Char c -> Can.Char c
        Src.String c -> Can.String c
        Src.Float c -> Can.Float c
        Src.Int c -> Can.Int c