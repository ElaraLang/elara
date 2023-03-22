module Elara.AST.Frontend.Unlocated where

import Elara.AST.Name (MaybeQualified, ModuleName, Name, OpName, TypeName, VarName)
import Prelude hiding (Op, Type)
import Elara.AST.Frontend (Pattern'(VarPattern))

{- | Frontend AST without location information.
     Trees that grow was getting quite frustrating, so we're stuck with this for now.
     I apologise to future me.
-}
data Expr
    = Int Integer
    | Float Double
    | String Text
    | Char Char
    | Unit
    | Var (MaybeQualified VarName)
    | Constructor (MaybeQualified TypeName)
    | Lambda [Pattern] Expr
    | FunctionCall Expr Expr
    | If Expr Expr Expr
    | BinaryOperator BinaryOperator Expr Expr
    | List [Expr]
    | Match Expr [(Pattern, Expr)]
    | LetIn VarName [Pattern] Expr Expr
    | Let VarName [Pattern] Expr
    | Block (NonEmpty Expr)
    | InParens Expr
    deriving (Show, Eq)

data Pattern
    = VarPattern VarName
    | ConstructorPattern (MaybeQualified TypeName) [Pattern]
    | ListPattern [Pattern]
    | WildcardPattern
    | IntegerPattern Integer
    | FloatPattern Double
    | StringPattern Text
    | CharPattern Char
    deriving (Show, Eq)

data BinaryOperator
    = Op (MaybeQualified OpName)
    | Infixed (MaybeQualified VarName)
    deriving (Show, Eq)

data TypeAnnotation = TypeAnnotation Name Type
    deriving (Show, Eq)

data Type
    = TypeVar Text
    | FunctionType Type Type
    | UnitType
    | TypeConstructorApplication Type Type
    | UserDefinedType (MaybeQualified TypeName)
    | RecordType (NonEmpty (VarName, Type))
    deriving (Show, Eq)

data Declaration = Declaration
    { _declarationModule' :: ModuleName
    , _declarationName :: Name
    , _declarationBody :: DeclarationBody
    }
    deriving (Show, Eq)

data DeclarationBody
    = -- | let <p> = <e>
      Value
        { _expression :: Expr
        , _patterns :: [Pattern]
        }
    | -- | def <name> : <type>.
      ValueTypeDef TypeAnnotation
    | -- | type <name> = <type>
      TypeAlias Type
    deriving (Show, Eq)