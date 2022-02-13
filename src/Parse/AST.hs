module Parse.AST where

import Data.List (intercalate)

newtype Line
  = ExpressionL Expression
  deriving (Eq)

instance Show Line where
  show (ExpressionL e) = show e

data Separator = Separator deriving (Show)

data Identifier
  = NormalIdentifier String
  | OpIdentifier String
  deriving (Show, Eq)

identifierValue :: Identifier -> String
identifierValue (NormalIdentifier s) = s
identifierValue (OpIdentifier s) = s

-- Patterns that might be used in a let expression
data Pattern
  = IdentifierP Identifier -- let x = ...
  | FunctionP {functionName :: Identifier, functionArgs :: [Pattern]} -- Let f a b = ...
  | TupleP [Pattern] -- Let (x, y) = ...
  | WildP -- let _ = ...
  deriving (Show, Eq)

data Constant
  = IntC Integer
  | StringC String
  | UnitC
  deriving (Show, Eq)

data Expression
  = ConstE Constant
  | LetE Pattern Expression
  | IdentifierE Identifier
  | InfixApplicationE Identifier Expression Expression
  | FuncApplicationE Expression Expression
  | BlockE [Expression]
  | ListE [Expression]
  | IfElseE Expression Expression Expression
  | LambdaE Identifier Expression
  deriving (Eq)

instance Show Expression where
  show = showASTNode

showASTNode :: Expression -> String
showASTNode (FuncApplicationE a b) = "(" ++ showASTNode a ++ " " ++ showASTNode b ++ ")"
showASTNode (LetE pattern value) = "let " ++ showPattern pattern ++ " = " ++ showASTNode value
showASTNode (IdentifierE i) = showIdentifier i
showASTNode (ConstE val) = showConst val
showASTNode (BlockE expressions) = "{" ++ (intercalate "; " $ map showASTNode expressions) ++ "}"
showASTNode (InfixApplicationE op a b) = showASTNode a ++ " " ++ showIdentifier op ++ " " ++ showASTNode b
showASTNode (ListE expressions) = "[" ++ (intercalate ", " $ map showASTNode expressions) ++ "]"
showASTNode (IfElseE condition thenBranch elseBranch) = "if " ++ showASTNode condition ++ " then " ++ showASTNode thenBranch ++ " else " ++ showASTNode elseBranch

showConst :: Constant -> String
showConst (StringC s) = s
showConst (IntC i) = show i

showIdentifier (NormalIdentifier i) = i
showIdentifier (OpIdentifier i) = i

showPattern (IdentifierP p) = showIdentifier p
showPattern e = show e

--showPattern (FunctionP name args) = showIdentifier name ++ " " ++ (intercalate " " $ map showPattern args)
