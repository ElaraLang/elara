{
module Parser where
import qualified Lexer as L
}

%name parseElara
%tokentype { L.Token }

%token
   let { L.Let _ }
   int { L.Int _ $$ }
   string { L.Str _ $$ }
   identifier { L.Identifier _ $$ }
   eq { L.Eq _ }
   op { L.Operator _ $$ }
   '`' { L.Backtick _ }
%%

Expression :: { Expression }
Expression  : Constant {ConstE $1}
            | let Pattern eq Expression {LetE $2 $4}
            | Identifier {IdentifierE $1}
            | Expression Expression {FuncApplicationE $1 $2}
            | Expression Operator Expression {InfixApplicationE $2 $1 $3}

Identifier :: { Identifier }
Identifier : identifier { NormalIdentifier $1 }

Operator :: { Identifier }
Operator : op { OpIdentifier $1 }
           | '`'identifier'`' { OpIdentifier $2 }



Pattern :: { Pattern }
Pattern : Identifier { IdentifierP $1 }

Constant :: { Constant }
Constant : int { IntC $1 }
         | string { StringC $1 }



{

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
                deriving (Show, Eq)

happyError :: [L.Token] -> a
happyError tks = error ("Parse error at " ++ lcn ++ "\n")
	where
	lcn = 	case tks of
		  [] -> "end of file"
		  tk:_ -> "line " ++ show l ++ ", column " ++ show c
			where
			L.AlexPn _ l c = L.tokenPosition tk
}
