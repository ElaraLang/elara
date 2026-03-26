module Elara.AST.New.Extensions where

import Elara.AST.New.Types
import Elara.Data.AtLeast2List (AtLeast2List)

-- | Binary operator expression extension (Frontend, Desugared, Renamed)
data BinaryOperatorExtension loc p
    = BinaryOperatorExpression (BinaryOperator loc p) (Expr loc p) (Expr loc p)
    deriving (Generic)

-- | Parenthesized expression extension (Frontend, Desugared, Renamed)
newtype InParensExtension loc p = InParensExpression (Expr loc p)
    deriving (Generic)

-- | List literal expression extension (Frontend, Desugared)
newtype ListExprExtension loc p = ListExpression [Expr loc p]
    deriving (Generic)

-- | Tuple literal expression extension (Frontend, Desugared)
newtype TupleExprExtension loc p = TupleExpression (AtLeast2List (Expr loc p))
    deriving (Generic)

-- | List/tuple/cons pattern extensions (Frontend, Desugared)
data ListTuplePatternExtension loc p
    = ListPattern [Pattern loc p]
    | TuplePattern (NonEmpty (Pattern loc p))
    | ConsPattern (Pattern loc p) (Pattern loc p)
    deriving (Generic)

-- | Tuple type extension (Frontend, Desugared)
newtype TupleTypeExtension loc p = TupleType (AtLeast2List (Type loc p))
    deriving (Generic)
