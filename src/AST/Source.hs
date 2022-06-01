module AST.Source where

import Elara.Name (Name)
import qualified Elara.Name as EN
import qualified Elara.String as ES

data Expr
  = Char Char
  | String ES.String
  | Float Float
  | Int Int
  | Var Name
  | List [Expr]
  | Op Name
  | Negate Expr
  | BinOp Expr Expr Expr
  | Lambda [Pattern] Expr
  | FunctionCall Expr [Expr]
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
  | PFloat Float -- 1.0
  deriving (Show)

data Def
  = Define EN.Name [Pattern]
  | Destruct Pattern
  deriving (Show)

data Type
  = TLambda Type Type -- a -> b
  | TVar Name -- a
  | TUnit -- ()
  | TTuple Type Type [Type] -- (Int, Float, String)
  | TConstructorApp Type Type [Type] -- Maybe A
  deriving (Show)

-- MODULE

data Module = Module
  { _name :: Maybe Name,
    _values :: [Value]
  }

data Value = Value Name [Pattern] Expr (Maybe Type) deriving (Show)