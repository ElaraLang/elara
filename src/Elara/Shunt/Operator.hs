module Elara.Shunt.Operator (OpTable, Associativity (..), Precedence, mkPrecedence, OpInfo (..), prettyOp, prettyOpTable) where

import Data.Map qualified as Map
import Elara.AST.Name
import Elara.AST.Phases.Renamed qualified as NewR
import Elara.AST.Region (SourceRegion)
import Elara.AST.Types qualified as New
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

prettyOp :: New.BinaryOperator SourceRegion NewR.Renamed -> Doc AnsiStyle
prettyOp (New.SymOp _ opRef) = Style.operator $ pretty opRef
prettyOp (New.InfixedOp _ vn) = Style.operator $ "`" <> pretty vn <> "`"

prettyOpTable :: OpTable -> Doc AnsiStyle
prettyOpTable table = brackets (vsep (map prettyOpInfo (Map.toList table)))
  where
    prettyOpInfo :: (IgnoreLocVarRef Name, OpInfo) -> Doc AnsiStyle
    prettyOpInfo (opName, info) =
        pretty info <+> pretty opName
