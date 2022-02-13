{
module Parse.Parser where
import Parse.Lexer
import Parse.Reader
import Parse.Utils
import Parse.Token
import Control.Monad.State.Lazy
import Parse.AST
import Debug.Trace
}

%nonassoc int string identifier let op if'`' '['
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
            | if Expression then Expression else Expression {IfElseE $2 $4 $6}

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
Pattern : Identifier FunctionPattern { FunctionP $1 $2 }
        | Identifier { IdentifierP $1 }

FunctionPattern :: { [Pattern] }
FunctionPattern : Pattern { [$1] }
                | Pattern FunctionPattern { $1 : $2 }

Constant :: { Constant }
Constant : int { IntC $1 }
         | string { StringC $1 }


Separator : newLine { addSemicolon }
          | semiColon { return Separator }
          | Separator Separator { return Separator }

Block :: { [Expression] }
Block : Expression { [$1] }
      | Separator indent BlockBody dedent { traceName "BlockBody" $3 }

BlockBody :: { [Expression] }
BlockBody : ExpressionWithSep { traceName "expression" [$1] }
          | BlockBody ExpressionWithSep { traceName "bb" ($2 : $1) }
          | {- empty -} { error "Empty block" }

ExpressionWithSep :: { Expression }
ExpressionWithSep : Expression Separator { $1 }

Line : Expression Separator { ExpressionL $1 }

Body :: { [Line] }
Body : Line { traceName "singleLine" [$1]}
     | Body Line { traceName "body line" ($2 : $1) }
     | {- empty -} { [] }
{

-- DEBUG
-- traceName name s = trace (name ++ " = " ++ show s) s
traceName _ s = s

parseError tok = do
  lno <- getLineNo
  colno <- getColNo
  s <- get
  error $ "Parse error on line " ++ show lno ++ ", column " ++ show colno ++ "." ++ "  " ++ show s ++ "\nat token " ++ show tok

addSemicolon :: P Separator
addSemicolon = do
  s@(ParseState _ _ _ pending _ _) <- get
  put $ s { pending_tokens = [SemiColon] ++ pending }
  return Separator

parse s = reverse $ evalP parseElara s

}
