module Elara.Shunt.Operator (OpTable, Associativity (..), Precedence, mkPrecedence, OpInfo (..), prettyOp, prettyOpTable) where

import Data.Map qualified as Map
import Elara.AST.Generic.Types (BinaryOperator (..), BinaryOperator' (..))
import Elara.AST.Name
import Elara.AST.Region (unlocated)
import Elara.AST.Renamed
import Elara.AST.VarRef
import Elara.Data.Pretty
import Elara.Data.Pretty.Styles qualified as Style

type OpTable = Map (IgnoreLocVarRef Name) OpInfo

newtype Precedence = Precedence Int
    deriving (Show, Eq, Ord)

instance Pretty Precedence where
    pretty (Precedence i) = pretty i

mkPrecedence :: Int -> Precedence
mkPrecedence i
    | i < 0 = error "Precedence must be positive"
    | i > 9 = error "Precedence must be less than 10"
    | otherwise = Precedence i

data OpInfo = OpInfo
    { precedence :: !Precedence
    , associativity :: !Associativity
    }
    deriving (Show, Eq, Ord)

instance Pretty OpInfo where
    pretty (OpInfo p LeftAssociative) = Style.keyword "infixl" <+> pretty p
    pretty (OpInfo p RightAssociative) = Style.keyword "infixr" <+> pretty p
    pretty (OpInfo p NonAssociative) = Style.keyword "infix" <+> pretty p

data Associativity
    = LeftAssociative
    | RightAssociative
    | NonAssociative
    deriving (Show, Eq, Ord)

prettyOp :: RenamedBinaryOperator -> Doc AnsiStyle
prettyOp (MkBinaryOperator op') = Style.operator $ case op' ^. unlocated of
    SymOp opName -> pretty (opName ^. unlocated)
    Infixed vn -> "`" <> pretty vn <> "`"

prettyOpTable :: OpTable -> Doc AnsiStyle
prettyOpTable table = brackets (vsep (map prettyOpInfo (Map.toList table)))
  where
    prettyOpInfo :: (IgnoreLocVarRef Name, OpInfo) -> Doc AnsiStyle
    prettyOpInfo (opName, info) =
        pretty info <+> pretty opName
