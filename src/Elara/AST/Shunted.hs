{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Elara.AST.Shunted where

import Control.Lens (Plated, concatMapOf, cosmosOn, makeLenses, makePrisms, view, (^.))
import Data.Data (Data)
import Elara.AST.Name
import Elara.AST.Region (Located (..), unlocated)
import Elara.AST.Renamed qualified as Renamed
import Elara.AST.VarRef
import Elara.Data.TopologicalGraph (HasDependencies (..))
import Elara.Data.Unique (Unique)
import Prelude hiding (Op)

{- | Shunted AST Type
This is very similar to 'Elara.AST.Renamed.Expr' except:

- Operators are re-shunted to match their defined precedence and associativity
- This means there's no need for an 'InParens' token anymore so that's also gone :D
- The confusing 'VarName'/'Elara.AST.Name.OpName' bs is also gone. Binary operator invocations are replaced with prefix function calls. This always uses VarName
-}
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
    = VarPattern (Located (VarRef VarName))
    | ConstructorPattern (Located (Qualified TypeName)) [Pattern]
    | ListPattern [Pattern]
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
    { _declaration'Module' :: Located ModuleName
    , _declaration'Name :: Located (Qualified Name)
    , _declaration'Body :: DeclarationBody
    }
    deriving (Show, Eq)

newtype DeclarationBody = DeclarationBody (Located DeclarationBody')
    deriving (Show, Eq)

data DeclarationBody'
    = -- | def <name> : <type> and / or let <p> = <e>
      Value
        { _expression :: Expr
        -- ^ The expression
        , _valueType :: Maybe (Located Renamed.Type)
        -- ^ An optional type annotation for the expression
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
    dependencies decl = case decl ^. _Declaration . unlocated . declaration'Body . _DeclarationBody . unlocated of
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
