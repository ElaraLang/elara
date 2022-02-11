{
module Parse.Parser where
import Parse.Lexer
import Parse.Utils
import Parse.Token
import Control.Monad.State.Lazy
}

%nonassoc int string identifier let op '`'
%nonassoc APP


%name parseElara Body
%tokentype { Token }
%monad { P }
%lexer { lexer } { EOF }
%error { parseError }



%token
   let { Let }
   if { If }
   then { Then }
   else { Else }
   int { Int $$ }
   string { Str $$ }
   identifier { Identifier $$ }
   eq { Eq }
   op { Operator $$ }
   '`' { Backtick }
   newLine { NewLine }
   semiColon { SemiColon }
   indent { Indent }
   dedent { Dedent }
   '[' { LSParen }
   ']' { RSParen }
   ',' { Comma }
%%

Expression :: { Expression }
Expression  : Constant {ConstE $1}
            | let Pattern eq Block {LetE $2 (BlockE $ reverse $4) }
            | Identifier {IdentifierE $1}
            | Expression Expression %prec APP { FuncApplicationE $1 $2 }
            | Expression Operator Expression {InfixApplicationE $2 $1 $3}
            | ListExpression {$1}

ListExpression :: { Expression }
ListExpression : '[' ListBody ']' {ListE $ reverse $2}

ListBody : ListBody ',' Expression { $3 : $1 }
      | ListBody ',' { $1 }
      | Expression { [$1] }
      | {- empty -} { [] }



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


Separator : newLine { [] }
          | semiColon { [] }

Block :: { [Expression] }
Block : BlockBody { $1 }
      | Separator Block { $2 }
      | indent BlockBody dedent { $2 }

BlockBody :: { [Expression] }
BlockBody : BlockBody Separator Expression { $3 : $1 }
          | BlockBody Separator { $1 }
          | Expression { [$1] }

Line : Expression { ExpressionL $1 }

Body :: { [Line] }
Body : Body Separator Line { $3 : $1 }
      | Body Separator { $1 }
      | Line { [$1] }
      | {- empty -} { [] }

{

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
                deriving (Show, Eq)

parseError tok = do
  lno <- getLineNo
  colno <- getColNo
  s <- get
  error $ "Parse error on line " ++ show lno ++ ", column " ++ show colno ++ "." ++ "  " ++ show s ++ "\nat token " ++ show tok


parse s = evalP parseElara s

}
