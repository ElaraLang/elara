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
  deriving (Eq)

instance Show Identifier where
  show (SimpleIdentifier s) = s
  show (OperatorIdentifier s) = s

data Pattern
  = IdentifierPattern Identifier
  | ConsPattern Pattern Pattern
  | ConstantPattern Constant
  | ListPattern [Pattern]
  | WildcardPattern
  deriving (Eq)

instance Show Pattern where
  show (IdentifierPattern i) = show i
  show WildcardPattern = "_"
  show (ConstantPattern c) = show c
  show (ConsPattern p1 p2) = "(" ++ show p1 ++ ":" ++ show p2 ++ ")"
  show (ListPattern ps) = "[" ++ intercalate "," (map show ps) ++ "]"

data Expression
  = Constant Constant
  | Reference Identifier
  | Lambda Identifier Expression Bool -- True if it's a recursive lambda. This also signals that the first parameter will be the function name
  | BindWithBody Identifier Expression Expression
  | BindGlobal Identifier Expression
  | Block [Expression]
  | FunctionApplication Expression Expression
  | List [Expression]
  | Cons Expression Expression
  | IfElse Expression Expression Expression
  | Match Expression [MatchCase]
  | Fix Expression -- Fix point operator, only used internally
  deriving (Eq)

instance Show Expression where
  show (Constant c) = show c
  show (Reference i) = show i
  show (Lambda p e _) = "\\" ++ show p ++ " -> " ++ show e
  show (BindWithBody p e1 e2) = "let " ++ show p ++ " = " ++ show e1 ++ " in " ++ show e2
  show (BindGlobal p e) = "let " ++ show p ++ " = " ++ show e
  show (Block es) = "{" ++ intercalate "; " (map show es) ++ "}"
  show (FunctionApplication e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
  show (List es) = "[" ++ intercalate ", " (map show es) ++ "]"
  show (Cons e1 e2) = show e1 ++ ":" ++ show e2
  show (IfElse e1 e2 e3) = "if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show e3
  show (Match e ms) = "match " ++ show e ++ " { " ++ intercalate "\n" (map show ms) ++ " }"
  show (Fix e) = "fix " ++ show e

data MatchCase = MatchCase Pattern Expression deriving (Eq)

instance Show MatchCase where
  show (MatchCase pattern value) = show pattern ++ " -> " ++ show value

matchCaseExpression :: MatchCase -> Expression
matchCaseExpression (MatchCase _ value) = value

data Line
  = ExpressionLine Expression
  | DefLine Identifier Type
  | TypeDefLine TypeDefinition
  deriving (Eq)

instance Show Line where
  show (ExpressionLine e) = show e
  show (DefLine p t) = "def " ++ show p ++ " : " ++ show t
  show (TypeDefLine t) = "type " ++ show t

data TypeDefinition = TypeDefinition TypeIdentifier [TypeVariable] TypeDefinitionBody deriving (Eq)

instance Show TypeDefinition where
  show (TypeDefinition tId tvs tBody) = show tId ++ " " ++ show tvs ++ " = " ++ show tBody

data TypeDefinitionBody
  = AliasType Type
  | TypeVariableType TypeVariable
  | UnionType TypeDefinitionBody TypeDefinitionBody
  | TypeConstructor TypeIdentifier [TypeDefinitionBody]
  | TypeConstructorInvocation Type [TypeDefinitionBody] -- Type constructor invocations
  deriving (Eq, Show)

newtype TypeIdentifier = TypeIdentifier String deriving (Eq)

instance Show TypeIdentifier where
  show (TypeIdentifier s) = s

newtype TypeVariable = TypeVariable String deriving (Eq)

instance Show TypeVariable where
  show (TypeVariable s) = s

data Type
  = NamedType String
  | VarType TypeVariable
  | ListType Type
  | UnitType
  | PureFunctionType Type Type
  | ImpureFunctionType Type Type
  deriving (Eq)

instance Show Type where
  show UnitType = "()"
  show (NamedType s) = s
  show (VarType s) = show s
  show (ListType t) = "[" ++ show t ++ "]"
  show (PureFunctionType t1 t2) = show t1 ++ " -> " ++ show t2
  show (ImpureFunctionType t1 t2) = show t1 ++ " => " ++ show t2
