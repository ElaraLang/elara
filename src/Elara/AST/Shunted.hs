{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Elara.AST.Shunted where

import Control.Lens (makeLenses, makePrisms)
import Elara.AST.Name
import Elara.AST.Region (IgnoreLocation (..), Located (..))
import Elara.Data.Pretty
import Elara.Data.Unique
import Elara.Data.Unwrap (Unwrap (unwrap))
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
    | Constructor (Located (VarRef TypeName))
    | Lambda (Located (Unique VarName)) Expr
    | FunctionCall Expr Expr
    | If Expr Expr Expr
    | List [Expr]
    | Match Expr [(Pattern, Expr)]
    | LetIn (Located (Unique VarName)) Expr Expr
    | Let (Located (Unique VarName)) Expr
    | Block (NonEmpty Expr)
    | Tuple (NonEmpty Expr)
    deriving (Show, Eq)

newtype Expr = Expr (Located Expr')
    deriving (Show, Eq)

data VarRef' c n
    = Global (c (Qualified n))
    | Local (c (Unique n))
    deriving (Functor)

deriving instance (Show (c (Qualified n)), Show (c (Unique n))) => Show (VarRef' c n)
deriving instance (Eq (c (Qualified n)), Eq (c (Unique n))) => Eq (VarRef' c n)

type VarRef n = VarRef' Located n

type IgnoreLocVarRef n = VarRef' IgnoreLocation n

mkLocal :: (ToName n) => Located (Unique n) -> VarRef Name
mkLocal n = Local (toName <<$>> n)

mkLocal' :: (ToName n) => Located (Unique n) -> IgnoreLocVarRef Name
mkLocal' n = Local (toName <<$>> IgnoreLocation n)

mkGlobal :: (ToName n) => Located (Qualified n) -> VarRef Name
mkGlobal n = Global (toName <<$>> n)

mkGlobal' :: (ToName n) => Located (Qualified n) -> IgnoreLocVarRef Name
mkGlobal' n = Global (toName <<$>> IgnoreLocation n)

withName :: (ToName n) => VarRef n -> VarRef Name
withName (Global n) = Global (toName <<$>> n)
withName (Local n) = Local (toName <<$>> n)

withName' :: (ToName n) => VarRef n -> IgnoreLocVarRef Name
withName' (Global n) = Global (toName <<$>> IgnoreLocation n)
withName' (Local n) = Local (toName <<$>> IgnoreLocation n)

data Pattern'
    = VarPattern (Located (VarRef VarName))
    | ConstructorPattern (Located (Qualified TypeName)) [Pattern]
    | ListPattern [Pattern]
    | WildcardPattern
    | IntegerPattern Integer
    | FloatPattern Double
    | StringPattern Text
    | CharPattern Char
    deriving (Show, Eq)

newtype Pattern = Pattern (Located Pattern')
    deriving (Show, Eq)

data TypeAnnotation = TypeAnnotation (Located (Qualified Name)) Type
    deriving (Show, Eq)

data Type
    = TypeVar (Unique LowerAlphaName)
    | FunctionType Type Type
    | UnitType
    | TypeConstructorApplication Type Type
    | UserDefinedType (Located (Qualified TypeName))
    | RecordType (NonEmpty (Located VarName, Type))
    | TupleType (NonEmpty Type)
    deriving (Show, Eq)

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
        , _valueType :: Maybe (Located TypeAnnotation)
        }
    | -- | type <name> = <type>
      TypeAlias (Located Type)
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

instance (Unwrap c, Pretty n) => Pretty (VarRef' c n) where
    pretty (Global n) = pretty (unwrap n)
    pretty (Local n) = pretty (unwrap n)