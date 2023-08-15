{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Elara.AST.Frontend where

import Control.Lens.TH
import Elara.AST.Name (LowerAlphaName, MaybeQualified, ModuleName, Name, OpName, TypeName, VarName)
import Elara.AST.Region (Located (..))
import Elara.AST.Unlocated.Frontend qualified as Unlocated
import Elara.AST.StripLocation (StripLocation (..))
import Prelude hiding (Op)

-- | Frontend AST
data Expr'
    = Int Integer
    | Float Double
    | String Text
    | Char Char
    | Unit
    | Var (Located (MaybeQualified VarName))
    | Constructor (Located (MaybeQualified TypeName))
    | Lambda [Pattern] Expr
    | FunctionCall Expr Expr
    | If Expr Expr Expr
    | BinaryOperator BinaryOperator Expr Expr
    | List [Expr]
    | Match Expr [(Pattern, Expr)]
    | LetIn (Located VarName) [Pattern] Expr Expr
    | Let (Located VarName) [Pattern] Expr
    | Block (NonEmpty Expr)
    | InParens Expr
    | Tuple (NonEmpty Expr)
    deriving (Show, Eq)

newtype Expr = Expr (Located Expr')
    deriving (Show, Eq)

data Pattern'
    = VarPattern (Located VarName)
    | ConstructorPattern (Located (MaybeQualified TypeName)) [Pattern]
    | ListPattern [Pattern]
    | ConsPattern Pattern Pattern
    | WildcardPattern
    | IntegerPattern Integer
    | FloatPattern Double
    | StringPattern Text
    | CharPattern Char
    deriving (Show, Eq)

newtype Pattern = Pattern (Located Pattern')
    deriving (Show, Eq)

data BinaryOperator'
    = Op (Located (MaybeQualified OpName))
    | Infixed (Located (MaybeQualified VarName))
    deriving (Show, Eq)

newtype BinaryOperator = MkBinaryOperator (Located BinaryOperator')
    deriving (Show, Eq)

data Type
    = TypeVar (Located LowerAlphaName)
    | FunctionType (Located Type) (Located Type)
    | UnitType
    | TypeConstructorApplication (Located Type) (Located Type)
    | UserDefinedType (Located (MaybeQualified TypeName))
    | RecordType (NonEmpty (Located VarName, Located Type))
    | TupleType (NonEmpty (Located Type))
    | ListType (Located Type)
    deriving (Show, Eq)

newtype Declaration = Declaration (Located Declaration')
    deriving (Show, Eq)

data Declaration' = Declaration'
    { _declaration'Module' :: Located ModuleName
    , _declaration'Name :: Located Name
    , _declaration'Body :: Located DeclarationBody
    }
    deriving (Show, Eq)

newtype DeclarationBody = DeclarationBody (Located DeclarationBody')
    deriving (Show, Eq)

data DeclarationBody'
    = -- | let <p> = <e>
      Value
        { _expression :: Expr
        , _patterns :: [Pattern]
        }
    | -- | def <name> : <type>.
      ValueTypeDef (Located Type)
    | -- | type <name> <vars> = <type>
      TypeDeclaration [Located LowerAlphaName] (Located TypeDeclaration)
    deriving (Show, Eq)

data TypeDeclaration
    = ADT (NonEmpty (Located TypeName, [Located Type]))
    | Alias (Located Type)
    deriving (Show, Eq)

makeLenses ''Declaration'
makeClassy ''Declaration
makeClassy ''DeclarationBody'
makePrisms ''Declaration
makePrisms ''DeclarationBody
makePrisms ''TypeDeclaration
makePrisms ''Expr
makePrisms ''Pattern

instance StripLocation Expr Unlocated.Expr where
    stripLocation (Expr (Located _ expr)) = case expr of
        Int i -> Unlocated.Int i
        Float f -> Unlocated.Float f
        String s -> Unlocated.String s
        Char c -> Unlocated.Char c
        Unit -> Unlocated.Unit
        Var v -> Unlocated.Var (stripLocation v)
        Constructor c -> Unlocated.Constructor (stripLocation c)
        Lambda p e -> Unlocated.Lambda (stripLocation p) (stripLocation e)
        FunctionCall e1 e2 -> Unlocated.FunctionCall (stripLocation e1) (stripLocation e2)
        If e1 e2 e3 -> Unlocated.If (stripLocation e1) (stripLocation e2) (stripLocation e3)
        BinaryOperator o e1 e2 -> Unlocated.BinaryOperator (stripLocation o) (stripLocation e1) (stripLocation e2)
        List l -> Unlocated.List (stripLocation l)
        Match e m -> Unlocated.Match (stripLocation e) (stripLocation m)
        LetIn v p e1 e2 -> Unlocated.LetIn (stripLocation v) (stripLocation p) (stripLocation e1) (stripLocation e2)
        Let v p e -> Unlocated.Let (stripLocation v) (stripLocation p) (stripLocation e)
        Block b -> Unlocated.Block (stripLocation b)
        InParens e -> Unlocated.InParens (stripLocation e)
        Tuple l -> Unlocated.Tuple (stripLocation l)

instance StripLocation Pattern Unlocated.Pattern where
    stripLocation (Pattern (Located _ pat)) = case pat of
        VarPattern n -> Unlocated.VarPattern (stripLocation n)
        ConstructorPattern c p -> Unlocated.ConstructorPattern (stripLocation c) (stripLocation p)
        ListPattern p -> Unlocated.ListPattern (stripLocation p)
        ConsPattern p1 p2 -> Unlocated.ConsPattern (stripLocation p1) (stripLocation p2)
        WildcardPattern -> Unlocated.WildcardPattern
        IntegerPattern i -> Unlocated.IntegerPattern i
        FloatPattern f -> Unlocated.FloatPattern f
        StringPattern s -> Unlocated.StringPattern s
        CharPattern c -> Unlocated.CharPattern c

instance StripLocation BinaryOperator Unlocated.BinaryOperator where
    stripLocation (MkBinaryOperator (Located _ op)) = case op of
        Op o -> Unlocated.Op (stripLocation o)
        Infixed i -> Unlocated.Infixed (stripLocation i)

instance StripLocation Type Unlocated.Type where
    stripLocation (TypeVar t) = Unlocated.TypeVar (stripLocation t)
    stripLocation (FunctionType t1 t2) = Unlocated.FunctionType (stripLocation t1) (stripLocation t2)
    stripLocation UnitType = Unlocated.UnitType
    stripLocation (TypeConstructorApplication t1 t2) = Unlocated.TypeConstructorApplication (stripLocation t1) (stripLocation t2)
    stripLocation (UserDefinedType t) = Unlocated.UserDefinedType (stripLocation t)
    stripLocation (RecordType r) = Unlocated.RecordType (stripLocation r)
    stripLocation (TupleType t) = Unlocated.TupleType (stripLocation t)
    stripLocation (ListType t) = Unlocated.ListType (stripLocation t)

instance StripLocation Declaration Unlocated.Declaration where
    stripLocation (Declaration d) = stripLocation d

instance StripLocation Declaration' Unlocated.Declaration where
    stripLocation (Declaration' m n b) = Unlocated.Declaration (stripLocation m) (stripLocation n) (stripLocation b)

instance StripLocation DeclarationBody Unlocated.DeclarationBody where
    stripLocation (DeclarationBody d) = stripLocation d

instance StripLocation DeclarationBody' Unlocated.DeclarationBody where
    stripLocation (Value e p) = Unlocated.Value (stripLocation e) (stripLocation p)
    stripLocation (ValueTypeDef t) = Unlocated.ValueTypeDef (stripLocation t)
    stripLocation (TypeDeclaration args t) = Unlocated.TypeDeclaration (stripLocation args) (stripLocation t)

instance StripLocation TypeDeclaration Unlocated.TypeDeclaration where
    stripLocation (ADT t) = Unlocated.ADT (stripLocation t)
    stripLocation (Alias t) = Unlocated.Alias (stripLocation t)