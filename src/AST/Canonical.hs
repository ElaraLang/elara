module AST.Canonical where

import Elara.ModuleName qualified as ModuleName
import Elara.Name
import Elara.String qualified as ES

data Module = Module
  { _name :: ModuleName.Canonical,
    _decls :: Decls
  }
  deriving (Show)

type Decls = [Def]

data Def
  = Def Name [Pattern] Expr -- The type needs to be inferred
  | TypedDef Name [Pattern] Expr Type -- Type explicitly stated with a def x : T, but still needs to be type checked!
  deriving (Show)

defName :: Def -> Name
defName (Def n _ _) = n
defName (TypedDef n _ _ _) = n

defPatterns :: Def -> [Pattern]
defPatterns (Def _ ps _) = ps
defPatterns (TypedDef _ ps _ _) = ps

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

data Type
  = TVar Name -- a
  | TLambda Type Type -- a -> b
  | TUnit -- ()
  | -- | TTuple Type Type [Type] -- (Int, Float, String)
    TConstructorApp Type Type -- Maybe A
  deriving (Show)
