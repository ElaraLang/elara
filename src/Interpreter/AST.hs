module Interpreter.AST where

import Data.List (intercalate)

data Constant
  = IntC Integer
  | StringC String
  | UnitC
  deriving (Eq)

instance Show Constant where
  show (IntC i) = show i
  show (StringC s) = show s
  show UnitC = "()"



data Identifier
  = SimpleIdentifier String
  | OperatorIdentifier String
  | TypeIdentifier String
  deriving (Eq)

instance Show Identifier where
  show (SimpleIdentifier s) = s
  show (OperatorIdentifier s) = s
  show (TypeIdentifier s) = s

data Pattern
  = IdentifierPattern Identifier
  | ConsPattern Pattern Pattern
  | ConstantPattern Constant
  | WildcardPattern
  deriving (Eq)

instance Show Pattern where
  show (IdentifierPattern i) = show i
  show WildcardPattern = "_"
  show (ConstantPattern c) = show c
  show (ConsPattern p1 p2) = "(" ++ show p1 ++ "::" ++ show p2 ++ ")"

data Expression
  = Constant Constant
  | Reference Identifier
  | Lambda Pattern Expression
  | Bind Pattern Expression
  | BindWithBody Pattern Expression Expression
  | Block [Expression]
  | FunctionApplication Expression Expression
  | List [Expression]
  | Cons Expression Expression
  | IfElse Expression Expression Expression
  | Match Expression [MatchCase]
  deriving (Eq)

instance Show Expression where
  show (Constant c) = show c
  show (Reference i) = show i
  show (Lambda p e) = "\\" ++ show p ++ " -> " ++ show e
  show (Bind p e) = "let " ++ show p ++ " = " ++ show e
  show (BindWithBody p e1 e2) = "let " ++ show p ++ " = " ++ show e1 ++ " in " ++ show e2
  show (Block es) = "{" ++ intercalate "; " (map show es) ++ "}"
  show (FunctionApplication e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
  show (List es) = "[" ++ intercalate ", " (map show es) ++ "]"
  show (Cons e1 e2) = show e1 ++ " :: " ++ show e2
  show (IfElse e1 e2 e3) = "if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show e3
  show (Match e ms) = "match " ++ show e ++ " { " ++ intercalate "\n" (map show ms) ++ " }"

data MatchCase = MatchCase Pattern Expression deriving (Eq)

instance Show MatchCase where
  show (MatchCase pattern value) = show pattern ++ " -> " ++ show value

newtype Line = ExpressionLine Expression deriving (Eq)

instance Show Line where
  show (ExpressionLine e) = show e
