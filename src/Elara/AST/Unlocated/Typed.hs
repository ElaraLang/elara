{-# LANGUAGE TemplateHaskell #-}

module Elara.AST.Unlocated.Typed where

import Control.Lens hiding (List)
import Control.Lens.Extras (uniplate)
import Data.Data (Data)
import Elara.AST.Name (LowerAlphaName, ModuleName, Name, Qualified (..), TypeName, VarName)
import Elara.Data.Pretty
import Elara.Data.Unique
import Elara.TypeInfer.Type (Type)
import Prelude hiding (Op)

-- | Typed AST Type without location information. See 'Elara.AST.Typed.Expr'' for the location information version.
data Expr'
    = Int Integer
    | Float Double
    | String Text
    | Char Char
    | Unit
    | Var (VarRef VarName)
    | Constructor (VarRef TypeName)
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

data VarRef n
    = Global (Qualified n)
    | Local (Unique n)
    deriving (Show, Eq, Ord, Functor, Data)

data Pattern'
    = VarPattern (VarRef VarName)
    | ConstructorPattern (Qualified TypeName) [Pattern]
    | ListPattern [Pattern]
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
    | NativeDef (TypeAnnotation)
    | -- | type <name> = <type>
      TypeAlias (Type ())
    deriving (Show, Eq)

makePrisms ''Declaration
makePrisms ''DeclarationBody
makeLenses ''DeclarationBody
makePrisms ''Expr
makePrisms ''Expr'
makePrisms ''VarRef
makePrisms ''Pattern

instance Pretty Expr
instance Pretty Expr'