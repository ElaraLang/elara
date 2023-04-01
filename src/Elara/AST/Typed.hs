{-# LANGUAGE TemplateHaskell #-}

module Elara.AST.Typed where

import Control.Lens hiding (List)
import Data.Data (Data)
import Elara.AST.Name (ModuleName, Name, Qualified, TypeName, VarName)
import Elara.AST.Region (Located, unlocated)
import Elara.Data.Unique
import Prelude hiding (Op, Type)

data PartialType = Id (Unique ()) | Partial (Type' PartialType) | Final Type
    deriving (Show, Eq, Ord)

{- | Typed AST Type
This is very similar to 'Elara.AST.Shunted.Expr' except:

- Everything has a type!
-}
data Expr' t
    = Int Integer
    | Float Double
    | String Text
    | Char Char
    | Unit
    | Var (Located (VarRef VarName))
    | Constructor (Located (VarRef TypeName))
    | Lambda (Located (Unique VarName)) (Expr t)
    | FunctionCall (Expr t) (Expr t)
    | If (Expr t) (Expr t) (Expr t)
    | List [Expr t]
    | Match (Expr t) [(Pattern t, Expr t)]
    | LetIn (Located (Unique VarName)) (Expr t) (Expr t)
    | Let (Located (Unique VarName)) (Expr t)
    | Block (NonEmpty (Expr t))
    deriving (Show, Eq, Data, Functor, Foldable, Traversable)

newtype Expr t = Expr (Located (Expr' t), t)
    deriving (Show, Eq, Data, Functor, Foldable, Traversable)

data VarRef n
    = Global (Located (Qualified n))
    | Local (Located (Unique n))
    deriving (Show, Eq, Ord, Functor, Data)

data Pattern' t
    = VarPattern (Located (VarRef VarName))
    | ConstructorPattern (Located (Qualified TypeName)) [Pattern t]
    | ListPattern [Pattern t]
    | WildcardPattern
    | IntegerPattern Integer
    | FloatPattern Double
    | StringPattern Text
    | CharPattern Char
    deriving (Show, Eq, Data, Functor, Foldable, Traversable)

newtype Pattern t = Pattern (Located (Pattern' t), t)
    deriving (Show, Eq, Data, Functor, Foldable, Traversable)

data TypeAnnotation t = TypeAnnotation (Located (Qualified Name)) t
    deriving (Show, Eq, Data)

data Type' t
    = TypeVar TypeVar
    | FunctionType t t
    | UnitType
    | TypeConstructorApplication t t
    | UserDefinedType (Located (Qualified TypeName))
    | RecordType (NonEmpty (Located VarName, t))
    deriving (Show, Eq, Ord)

newtype Type = Type (Type' Type)
    deriving (Show, Eq, Ord)

newtype TypeVar = TyVar (Unique Text)
    deriving (Show, Eq, Ord)

newtype Declaration t = Declaration (Located (Declaration' t))
    deriving (Show, Eq)

data Declaration' t = Declaration'
    { _declaration'Module' :: Located ModuleName
    , _declaration'Name :: Located (Qualified Name)
    , _declaration'Body :: DeclarationBody t
    }
    deriving (Show, Eq)

newtype DeclarationBody t = DeclarationBody (Located (DeclarationBody' t))
    deriving (Show, Eq)

data DeclarationBody' t
    = -- | def <name> : <type> and / or let <p> = <e>
      Value
        { _expression :: Expr t
        , _valueType :: Maybe (Located (TypeAnnotation t))
        }
    | NativeDef (Located (TypeAnnotation t))
    | -- | type <name> = <type>
      TypeAlias (Located Type)
    deriving (Show, Eq)

makePrisms ''Declaration
makeLenses ''Declaration'
makePrisms ''DeclarationBody
makePrisms ''DeclarationBody'
makeLenses ''DeclarationBody
makeLenses ''DeclarationBody'
makePrisms ''Expr
makePrisms ''Expr'
makePrisms ''VarRef
makePrisms ''Pattern

instance Plated (Expr t) where
    plate :: Traversal' (Expr t) (Expr t)
    plate = id

instance Plated (Expr' t) where
    plate :: Traversal' (Expr' t) (Expr' t)
    plate f = \case
        Int i -> pure (Int i)
        Float f' -> pure (Float f')
        String s -> pure (String s)
        Char c -> pure (Char c)
        Unit -> pure Unit
        Var v -> pure (Var v)
        Constructor c -> pure (Constructor c)
        Lambda p e -> Lambda p <$> traverseExpr' f e
        FunctionCall e1 e2 ->
            FunctionCall
                <$> traverseExpr' f e1
                <*> traverseExpr' f e2
        If e1 e2 e3 ->
            If
                <$> traverseExpr' f e1
                <*> traverseExpr' f e2
                <*> traverseExpr' f e3
        List es -> List <$> traverse (traverseExpr' f) es
        Match e pes ->
            Match
                <$> traverseExpr' f e
                <*> traverse
                    (traverseOf (_2 . _Expr . _1 . unlocated) f)
                    pes
        LetIn v e1 e2 ->
            LetIn v
                <$> traverseExpr' f e1
                <*> traverseExpr' f e2
        Let v e -> Let v <$> traverseExpr' f e
        Block es -> Block <$> traverse (traverseExpr' f) es
      where
        traverseExpr' :: Functor f => (Expr' t -> f (Expr' t)) -> Expr t -> f (Expr t)
        traverseExpr' = traverseOf (_Expr . _1 . unlocated)