{
module Parse.Parser where
import Parse.Lexer
}

%nonassoc int string identifier let op '`'
%nonassoc APP

%name parseElara
%tokentype { Token }
%monad { Alex } { >>= } { return }
%lexer { lexwrap } { EOF }
%error { parseError }



%token
   let { Let _ }
   int { Int _ $$ }
   string { Str _ $$ }
   identifier { Identifier _ $$ }
   eq { Eq _ }
   op { Operator _ $$ }
   '`' { Backtick _ }
%%

Expression :: { Expression }
Expression  : Constant {ConstE $1}
            | let Pattern eq Expression {LetE $2 $4}
            | Identifier {IdentifierE $1}
            | Expression Expression %prec APP { FuncApplicationE $1 $2 }
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

data Expression = ConstE Comnstant
                | LetE Pattern Expression
                | IdentifierE Identifier
                | InfixApplicationE Identifier Expression Expression
                | FuncApplicationE Expression Expression
                deriving (Show, Eq)

parseError _ = do
  ((AlexPn _ line column), _, _, _) <- alexGetInput
  alexError ("parse error at line " ++ (show line) ++ ", column " ++ (show column))

lexwrap = (alexMonadScan >>=)
}
