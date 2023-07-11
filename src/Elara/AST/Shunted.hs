{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Elara.AST.Shunted where

import Control.Lens (Plated, concatMapOf, cosmosOn, makeLenses, makePrisms, view, (^.))
import Data.Data (Data)
import Elara.AST.Lenses (HasDeclarationBody (..), HasDeclarationBody' (..))
import Elara.AST.Name
import Elara.AST.Pretty
import Elara.AST.Region (Located (..), unlocated)
import Elara.AST.Renamed qualified as Renamed
import Elara.AST.StripLocation
import Elara.AST.VarRef
import Elara.Data.Pretty
import Elara.Data.TopologicalGraph (HasDependencies (..))
import Elara.Data.Unique (Unique)
import Print (showColored, showPretty)
import Prelude hiding (Op)

-- | Shunted AST Type
-- This is very similar to 'Elara.AST.Renamed.Expr' except:
--
-- - Operators are re-shunted to match their defined precedence and associativity
-- - This means there's no need for an 'InParens' token anymore so that's also gone :D
-- - The confusing 'VarName'/'Elara.AST.Name.OpName' bs is also gone. Binary operator invocations are replaced with prefix function calls. This always uses VarName
data Expr'
  = Int Integer
  | Float Double
  | String Text
  | Char Char
  | Unit
  | Var (Located (VarRef VarName))
  | Constructor (Located (Qualified TypeName))
  | Lambda (Located (Unique VarName)) Expr
  | FunctionCall Expr Expr
  | If Expr Expr Expr
  | List [Expr]
  | Match Expr [(Pattern, Expr)]
  | LetIn (Located (Unique VarName)) Expr Expr
  | Let (Located (Unique VarName)) Expr
  | Block (NonEmpty Expr)
  | Tuple (NonEmpty Expr)
  deriving (Show, Eq, Data)

instance Plated Expr'

newtype Expr = Expr (Located Expr')
  deriving (Show, Eq, Data)

data Pattern'
  = VarPattern (Located (Unique VarName))
  | ConstructorPattern (Located (Qualified TypeName)) [Pattern]
  | ListPattern [Pattern]
  | ConsPattern Pattern Pattern
  | WildcardPattern
  | IntegerPattern Integer
  | FloatPattern Double
  | StringPattern Text
  | CharPattern Char
  deriving (Show, Eq, Data)

newtype Pattern = Pattern (Located Pattern')
  deriving (Show, Eq, Data)

newtype Declaration = Declaration (Located Declaration')
  deriving (Show, Eq)

data Declaration' = Declaration'
  { _declaration'Module' :: Located ModuleName,
    _declaration'Name :: Located (Qualified Name),
    _declaration'Body :: Located DeclarationBody
  }
  deriving (Show, Eq)

newtype DeclarationBody = DeclarationBody (Located DeclarationBody')
  deriving (Show, Eq)

data DeclarationBody'
  = -- | def <name> : <type> and / or let <p> = <e>
    Value
      { -- | The expression
        _expression :: Expr,
        -- | An optional type annotation for the expression
        _valueType :: Maybe (Located Renamed.Type)
      }
  | -- | type <name> <vars> = <type>
    TypeDeclaration [Located (Unique LowerAlphaName)] (Located Renamed.TypeDeclaration) -- No difference to old AST
  deriving (Show, Eq)

makePrisms ''Declaration
makeLenses ''Declaration'
makePrisms ''Declaration'
makePrisms ''DeclarationBody
makePrisms ''DeclarationBody'
makeLenses ''DeclarationBody
makeLenses ''DeclarationBody'
makePrisms ''Expr
makePrisms ''Pattern

instance HasDependencies Declaration where
  type Key Declaration = Qualified Name
  key = view (_Declaration . unlocated . declaration'Name . unlocated)
  dependencies decl = case decl ^. unlocatedDeclarationBody' of
    Value e _ -> valueDependencies e
    TypeDeclaration _ _ -> []

valueDependencies :: Expr -> [Qualified Name]
valueDependencies =
  concatMapOf (cosmosOn (_Expr . unlocated)) names
  where
    names :: Expr' -> [Qualified Name]
    names (Var (Located _ (Global e))) = NVarName <<$>> [e ^. unlocated]
    names (Constructor (Located _ e)) = [NTypeName <$> e]
    names _ = []

instance HasDeclarationBody Declaration (Located DeclarationBody) where
  declarationBody = _Declaration . unlocated . declarationBody
  unlocatedDeclarationBody = declarationBody . unlocated

instance HasDeclarationBody Declaration' (Located DeclarationBody) where
  declarationBody = declaration'Body
  unlocatedDeclarationBody = declarationBody . unlocated

instance HasDeclarationBody' Declaration (Located DeclarationBody') where
  declarationBody' = declarationBody . unlocated . _DeclarationBody
  unlocatedDeclarationBody' = declarationBody' . unlocated

instance HasDeclarationBody' Declaration' (Located DeclarationBody') where
  declarationBody' = declarationBody . unlocated . _DeclarationBody
  unlocatedDeclarationBody' = declarationBody' . unlocated

-- Pretty Printing :D

instance Pretty Declaration where
  pretty (Declaration ldb) = pretty ldb

instance Pretty Declaration' where
  pretty (Declaration' _ n b) = prettyDB (n ^. unlocated) (b ^. unlocated . _DeclarationBody . unlocated)

prettyDB :: Qualified Name -> DeclarationBody' -> Doc AnsiStyle
prettyDB n (Value e t) = prettyValueDeclaration n e t
prettyDB n (TypeDeclaration vars t) = prettyTypeDeclaration n vars t

instance Pretty Expr where
  pretty (Expr e) = pretty (stripLocation e)

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
  pretty (FunctionCall (Expr (Located _ (FunctionCall (Expr (Located _ (Var (Located _ fName)))) lr))) r)
    | (Located _ (OperatorVarName _)) <- varRefVal fName =
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
  pretty (Pattern p) = pretty (stripLocation p)

instance Pretty Pattern' where
  pretty (VarPattern v) = pretty v
  pretty (ConstructorPattern c ps) = prettyConstructorPattern c ps
  pretty (ListPattern l) = prettyListPattern l
  pretty (ConsPattern p1 p2) = prettyConsPattern p1 p2
  pretty other = show other
