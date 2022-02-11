module Parse.AST where


data Line =
   ExpressionL Expression
   deriving (Show, Eq)

data Identifier = NormalIdentifier String
                 | OpIdentifier String
                 deriving (Show, Eq)

data Pattern = IdentifierP Identifier
             | TupleP [Pattern]
             | WildP
             deriving (Show, Eq)

data Constant = IntC Int
              | StringC String
              | UnitC
              deriving (Show, Eq)

data Expression = ConstE Constant
                | LetE Pattern Expression
                | IdentifierE Identifier
                | InfixApplicationE Identifier Expression Expression
                | FuncApplicationE Expression Expression
                | BlockE [Expression]
                | ListE [Expression]
                | IfElseE Expression Expression Expression
                deriving (Show, Eq)

