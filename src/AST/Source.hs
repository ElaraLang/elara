module AST.Source where

import Data.Maybe (fromMaybe)
import Elara.Name (Name)
import Elara.Name qualified as EN
import Elara.Name qualified as Name
import Elara.String qualified as ES

data Expr
  = Char Char
  | String ES.String
  | Float Double
  | Int Int
  | Var Name
  | List [Expr]
  | Op Name
  | Negate Expr
  | BinOp Expr Expr Expr
  | Lambda [Pattern] Expr
  | FunctionCall Expr Expr
  | If Expr Expr Expr
  | Let Def Expr
  | LetIn Def Expr Expr
  | Match Expr [(Pattern, Expr)]
  | Unit
  | Tuple Expr Expr [Expr]
  | BlockExpr [Expr]
  deriving (Show)

data Pattern
  = PWildcard -- _
  | PVar Name -- x
  | PUnit -- ()
  | PList [Pattern] -- [x, y]
  | PBind Name Pattern -- a@pat
  | PCons Pattern Pattern -- a:b
  | PConstructor Name [Pattern] -- Foo (x y)
  -- These are used in match expressions only because they match a single element rather than a set
  | PChar Char -- 'a'
  | PString ES.String -- "a"
  | PInt Int -- 1
  | PFloat Double -- 1.0
  deriving (Show)

data Def
  = Define EN.Name [Pattern] Expr
  | Destruct Pattern Expr
  deriving (Show)

data Type
  = TLambda Type Type -- a -> b
  | TVar Name -- a
  | TCon Name -- Int
  | TUnit -- ()
  | TTuple Type Type [Type] -- (Int, Float, String)
  | TConstructorApp Type Type [Type] -- Maybe A
  deriving (Show)

-- MODULE

data Module = Module
  { _name :: Maybe Name,
    _exports :: Exposing,
    _imports :: [Import],
    _decls :: [Decl],
    _values :: [Value]
  }

data Decl = Decl Name Type deriving (Show) -- def name : Type

data Value = Value Name [Pattern] Expr deriving (Show) -- let name [params] = expr

getName :: Module -> Name
getName m = fromMaybe Name._Main (m._name)

data Import = Import
  { _import :: Name,
    _as :: Maybe Name,
    _exposing :: Exposing
  }
  deriving (Show)

data Exposing
  = Everything
  | Some Exposed
  deriving (Show)

type Exposed = [Name]
