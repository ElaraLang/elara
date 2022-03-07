module Parse.AST where

import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..), toList)

data Line
  = ExpressionL Expression
  | DefL Identifier Type
  | TypeDefL TypeDef
  deriving (Eq)

instance Show Line where
  show (ExpressionL e) = show e
  show (DefL p t) = "def " ++ show p ++ " : " ++ show t
  show (TypeDefL t) = show t

newtype TypeIdentifier = TypeIdentifier String deriving (Eq)

instance Show TypeIdentifier where
  show (TypeIdentifier s) = s

newtype TypeVariable = TypeVariable String deriving (Eq)

instance Show TypeVariable where
  show (TypeVariable s) = s

data TypeDef = TypeDef TypeIdentifier [TypeVariable] TypeDefBody deriving (Eq)

instance Show TypeDef where
  show (TypeDef ti vs t) = "type " ++ show ti ++ " " ++ unwords (show <$> vs) ++ " = " ++ show t

data TypeDefBody
  = AliasType Type
  | TypeVariableType TypeVariable
  | UnionType TypeDefBody TypeDefBody
  | TypeConstructor Type [TypeDefBody]
  | TypeConstructorInvocation TypeIdentifier [TypeDefBody] -- Type constructor invocations
  deriving (Eq, Show)

data Separator = Separator deriving (Show)

data Identifier
  = NormalIdentifier String
  | OpIdentifier String
  deriving (Eq)

instance Show Identifier where
  show (NormalIdentifier s) = s
  show (OpIdentifier s) = "(" ++ s ++ ")"

identifierValue :: Identifier -> String
identifierValue (NormalIdentifier s) = s
identifierValue (OpIdentifier s) = s

-- Patterns that might be used in a let expression
data Pattern
  = IdentifierP Identifier -- let x = ...
  | FunctionP {functionName :: Identifier, functionArgs :: [Pattern]} -- Let f a b = ...
  | ConsP Pattern Pattern -- let f (x::xs) = ...
  | ListP [Pattern] -- let [x, y] = ...
  | TupleP [Pattern] -- Let (x, y) = ...
  | ConstantP Constant -- Let 1 = ... Note that this doesn't actually work in let expressions, but is used in matches
  | WildP -- let _ = ...
  deriving (Eq, Show)

data Constant
  = IntC Integer
  | StringC String
  | UnitC
  deriving (Eq)

instance Show Constant where
  show (IntC i) = show i
  show (StringC s) = show s
  show UnitC = "()"

data Expression
  = ConstE Constant
  | LetE Pattern Expression
  | IdentifierE Identifier
  | InfixApplicationE Identifier Expression Expression
  | FuncApplicationE Expression Expression
  | BlockE (NonEmpty Expression)
  | ListE [Expression]
  | IfElseE Expression Expression Expression
  | LambdaE Pattern Expression
  | ConsE Expression Expression
  | MatchE Expression [MatchLine]
  deriving (Eq)

instance Show Expression where
  show = showASTNode

showASTNode :: Expression -> String
showASTNode (LambdaE p e) = "\\" ++ showPattern p ++ " -> " ++ show e
showASTNode (FuncApplicationE a b) = "(" ++ showASTNode a ++ " " ++ showASTNode b ++ ")"
showASTNode (LetE pattern value) = "let " ++ showPattern pattern ++ " = " ++ showASTNode value
showASTNode (IdentifierE i) = showIdentifier i
showASTNode (ConstE val) = show val
showASTNode (BlockE expressions) = "{" ++ (intercalate "; " $ map showASTNode $ toList expressions) ++ "}"
showASTNode (InfixApplicationE op a b) = showASTNode a ++ " " ++ showIdentifier op ++ " " ++ showASTNode b
showASTNode (ListE expressions) = "[" ++ (intercalate ", " $ map showASTNode expressions) ++ "]"
showASTNode (IfElseE condition thenBranch elseBranch) = "if " ++ showASTNode condition ++ " then " ++ showASTNode thenBranch ++ " else " ++ showASTNode elseBranch
showASTNode (ConsE a b) = showASTNode a ++ " : " ++ showASTNode b
showASTNode (MatchE expression matchLines) = "match " ++ showASTNode expression ++ " { " ++ (intercalate "\n" $ map show matchLines) ++ " } "

data MatchLine = MatchLine Pattern Expression deriving (Eq)

instance Show MatchLine where
  show (MatchLine pattern value) = showPattern pattern ++ " -> " ++ showASTNode value

showIdentifier :: Identifier -> String
showIdentifier (NormalIdentifier i) = i
showIdentifier (OpIdentifier i) = i

showPattern :: Pattern -> String
showPattern (IdentifierP p) = showIdentifier p
showPattern e = show e

--showPattern (FunctionP name args) = showIdentifier name ++ " " ++ (intercalate " " $ map showPattern args)

data Type
  = NamedT String
  | ListT Type
  | PureFunT Type Type
  deriving (Eq)

instance Show Type where
  show (NamedT s) = s
  show (ListT t) = "[" ++ show t ++ "]"
  show (PureFunT t1 t2) = show t1 ++ " -> " ++ show t2
