module AST.Source where

data Identifier
  = NormalIdentifier String
  | OpIdentifier String
  deriving (Show, Eq)

data Type
  = UnitType
  | TypeVariable Identifier
  | TupleType Type Type [Type]
  | Type String [Type]
  | PureFunctionType Type Type
  | ImpureFunctionType Type Type
  deriving (Show, Eq)

data Def = Def Identifier Type deriving (Show, Eq)

data Let = Let (Maybe Def) Identifier Expr deriving (Show, Eq)

data Line
  = DefLetLine Let
  | ExprLine Expr
  deriving (Show, Eq)

data Expr
  = StringE String
  | IntE Integer
  | FloatE Double
  | CharE Char
  | Block [Line]
  | LetE Let Expr Expr -- let a = b in c
  deriving (Show, Eq)
