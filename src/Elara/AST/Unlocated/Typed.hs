{-# LANGUAGE TemplateHaskell #-}

module Elara.AST.Unlocated.Typed where

import Control.Lens hiding (List, none)
import Elara.AST.Name (ModuleName, Name, Qualified (..), TypeName, VarName (..))
import Elara.AST.Pretty
import Elara.AST.Region (Located (..))
import Elara.AST.VarRef (UnlocatedVarRef, varRefVal)
import Elara.Data.Pretty
import Elara.Data.Unique
import Elara.TypeInfer.Type (Type)
import Prelude hiding (Op, group)

-- | Typed AST Type without location information. See 'Elara.AST.Typed.Expr'' for the location information version.
data Expr'
    = Int Integer
    | Float Double
    | String Text
    | Char Char
    | Unit
    | Var (UnlocatedVarRef VarName)
    | Constructor (Qualified TypeName)
    | Lambda (Unique VarName) Expr
    | FunctionCall Expr Expr
    | If Expr Expr Expr
    | List [Expr]
    | Match Expr [(Pattern, Expr)]
    | LetIn (Unique VarName) Expr Expr
    | Let (Unique VarName) Expr
    | Block (NonEmpty Expr)
    | Tuple (NonEmpty Expr)
    deriving (Show, Eq)

newtype Expr = Expr (Expr', Type ())
    deriving (Show, Eq)

data Pattern'
    = VarPattern (Unique VarName)
    | ConstructorPattern (Qualified TypeName) [Pattern]
    | ListPattern [Pattern]
    | ConsPattern Pattern Pattern
    | WildcardPattern
    | IntegerPattern Integer
    | FloatPattern Double
    | StringPattern Text
    | CharPattern Char
    deriving (Show, Eq)

newtype Pattern = Pattern (Pattern', Type ())
    deriving (Show, Eq)

data TypeAnnotation = TypeAnnotation (Qualified Name) (Type ())
    deriving (Show, Eq)
data Declaration = Declaration'
    { _declaration'Module' :: ModuleName
    , _declaration'Name :: Qualified Name
    , _declaration'Body :: DeclarationBody
    }
    deriving (Show, Eq)

data DeclarationBody
    = -- | def <name> : <type> and / or let <p> = <e>
      Value
        { _expression :: Expr
        }
    | NativeDef TypeAnnotation
    | -- | type <name> = <type>
      TypeAlias (Type ())
    deriving (Show, Eq)

makePrisms ''Declaration
makePrisms ''DeclarationBody
makeLenses ''DeclarationBody
makePrisms ''Expr
makePrisms ''Expr'
makePrisms ''Pattern

instance Pretty Expr where
    pretty (Expr (e, t)) = parensIf needsParens (group (flatAlt long short))
      where
        needsParens = case e of
            FunctionCall _ _ -> True
            If {} -> True
            Match _ _ -> True
            LetIn {} -> True
            Let _ _ -> True
            Block _ -> True
            _ -> False
        long = pretty e
        short = align (pretty e )

instance Pretty Expr' where
    pretty (Int i) = pretty i
    pretty (Float f) = pretty f
    pretty (String s) = pretty '\"' <> pretty s <> pretty '\"'
    pretty (Char c) = "'" <> escapeChar c <> "'"
    pretty Unit = "()"
    pretty (Var v) = pretty v
    pretty (Constructor c) = pretty c
    pretty (Lambda ps e) = prettyLambdaExpr [ps] e
    -- re-sugar infix operators
    pretty (FunctionCall (Expr (FunctionCall (Expr (Var fName, _)) lr, _)) r)
        | (Identity (OperatorVarName _)) <- varRefVal fName =
            prettyBinaryOperatorExpr lr fName r
    pretty (FunctionCall e1 e2) = prettyFunctionCallExpr e1 e2
    pretty (If e1 e2 e3) = prettyIfExpr e1 e2 e3
    pretty (List l) = prettyListExpr l
    pretty (Match e m) = prettyMatchExpr e (prettyMatchBranch <$> m)
    pretty (LetIn v e1 e2) = prettyLetInExpr v none e1 e2
    pretty (Let v e) = prettyLetExpr v none e
    pretty (Block b) = prettyBlockExpr b
    pretty other = show other

instance Pretty Pattern where
    pretty (Pattern (p, _)) = pretty p

instance Pretty Pattern' where
    pretty (VarPattern v) = pretty v
    pretty (ConstructorPattern c ps) = prettyConstructorPattern c ps
    pretty (ListPattern l) = prettyListPattern l
    pretty (ConsPattern p1 p2) = prettyConsPattern p1 p2
    pretty other = show other
