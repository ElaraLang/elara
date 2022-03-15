{
module Parse.Parser where
import Parse.Lexer
import Parse.Reader
import Parse.Utils
import Parse.Token as L
import Control.Monad.State.Lazy
import Parse.AST as P
import Debug.Trace
import Data.List.NonEmpty (fromList)
}

%right ':'
%nonassoc int string identifier let if '`' '[' '(' match def
%left op
%nonassoc APP


%name parseElara Body
%tokentype { Token }
%monad { P }
%lexer { lexer } { EOF }
%error { parseError }


%token
   let { Let }
   in { In }
   def { Def }
   if { If }
   then { Then }
   else { Else }
   match { Match }
   type { Type }
   int { Int $$ }
   string { Str $$ }
   identifier { Identifier $$ }
   typeIdentifier { L.TypeIdentifier $$ }
   eq { Eq }
   op { Operator $$ }
   '`' { Backtick }
   ':' { Colon }
   newLine { NewLine }
   semiColon { SemiColon }
   indent { Indent }
   dedent { Dedent }
   '(' { LParen }
   ')' { RParen }
   '[' { LSParen }
   ']' { RSParen }
   ',' { Comma }
   '_' { Wildcard }
   '->' { Arrow }
   '=>' { BigArrow }
   '|' { Pipe }
   '\\' { Backslash }
%%

Type :: { Type }
Type  : typeIdentifier { NamedT $1 }
      | identifier { VarT $1 }
      | '[' Type ']' { ListT $2 }
      | Type '->' Type { PureFunT $1 $3 }
      | Type '=>' Type { ImpureFunT $1 $3 }
      | '(' Type ')' { $2 }


TypeIdentifier : typeIdentifier { P.TypeIdentifier $1 }

Expression :: { Expression }
Expression  : Constant {ConstE $1}
            | let Pattern eq Block {LetE $2 $4 }
            | let Pattern eq Block in Expression {LetInE $2 $4 $6 }
            | Identifier {IdentifierE $1}
            | '(' Expression ')' { $2 }
            | Expression Expression %prec APP { FuncApplicationE $1 $2 }
            | Expression Operator Expression %prec APP {InfixApplicationE $2 $1 $3}
            | ListExpression {$1}
            | if Expression then Expression else Expression {IfElseE $2 $4 $6}
            | Expression ':' Expression {ConsE $1 $3}
            | MatchExpression { $1 }
            | '\\' Pattern '->' Block {LambdaE $2 $4 False}


ListExpression :: { Expression }
ListExpression : '[' ListBody ']' {ListE $ reverse $2}

ListBody : ListBody ',' Expression { $3 : $1 }
      | ListBody ',' { $1 }
      | Expression { [$1] }
      | {- empty -} { [] }

Identifier :: { Identifier }
Identifier : identifier { NormalIdentifier $1 }
           | '(' op ')' { NormalIdentifier $2 }

Operator :: { Identifier }
Operator : op { OpIdentifier $1 }
           | '`'identifier'`' { OpIdentifier $2 }

MatchExpression :: { Expression }
MatchExpression : match Expression MatchBlock {MatchE $2 $3}

MatchBlock :: { [MatchLine] }
MatchBlock : Separator indent MatchBody dedent { traceName "MatchBody" (reverse $3) }

MatchBody :: { [MatchLine] }
MatchBody : MatchLine { [$1] }
         | MatchBody MatchLine { $2 : $1 }

MatchLine : Pattern '->' Block Separator { MatchLine $1 $3 }


Pattern :: { Pattern }
Pattern : SingleValuePattern { $1 }
        | Identifier FunctionPattern  { FunctionP $1 $2 } -- Function pattern cannot be recursive

SingleValuePattern :: { Pattern }
SingleValuePattern : Identifier { IdentifierP $1 }
                   | Constant { ConstantP $1 }
                   | '_' { WildP }
                   | '(' SingleValuePattern ':' SingleValuePattern ')' { ConsP $2 $4 }
                   | '[' ListPattern ']' { ListP $ reverse $2 }

ListPattern :: { [Pattern] }
ListPattern : {- empty -} { [] }
            | SingleValuePattern { [$1] }
            | ListPattern ',' SingleValuePattern { $3 : $1 }

FunctionPattern :: { [Pattern] }
FunctionPattern : SingleValuePattern { [$1] }
                | SingleValuePattern FunctionPattern { $1 : $2 }

Constant :: { Constant }
Constant : int { IntC $1 }
         | string { StringC $1 }
         | '(' ')' { UnitC }


Separator : newLine { addSemicolon }
          | semiColon { return Separator }
          | Separator Separator { return Separator }

Block :: { Expression }
Block : Expression { $1 }
      | Separator indent BlockBody dedent { traceName "BlockBody" (BlockE $ fromList $ reverse $3) }

BlockBody :: { [Expression] }
BlockBody : ExpressionWithSep { traceName "expression" [$1] }
          | BlockBody ExpressionWithSep { traceName "bb" ($2 : $1) }
          | {- empty -} { error "Empty block" }

ExpressionWithSep :: { Expression }
ExpressionWithSep : Expression Separator { $1 }

TypeVariables :: { [TypeVariable] }
TypeVariables : TypeVariable TypeVariables { $1 : $2 }
              | {- empty -} { [] }

TypeVariable : identifier { TypeVariable $1 }

TypeDef :: { TypeDef }
TypeDef : type TypeIdentifier TypeVariables eq TypeDefBody { TypeDef $2 $3 $5 }
TypeDefBody : Type { AliasType $1 }
            | TypeVariable { TypeVariableType $1 }
            | '(' TypeDefBody ')' { $2 }
            | TypeIdentifier TypeDefBodyMany { TypeConstructor $1 $2 }
            | TypeDefBody '|' TypeDefBody { UnionType $1 $3 }

TypeDefBodyMany :: { [TypeDefBody] }
TypeDefBodyMany : {- empty -} { [] }
                | TypeDefBody TypeDefBodyMany { $1 : $2 }

Line : Expression Separator { ExpressionL $1 }
     | def Identifier ':' Type Separator { DefL $2 $4 }
     | TypeDef Separator { TypeDefL $1 }


Body :: { [Line] }
Body : Expression { traceName "singleExpr" [ExpressionL $1]}
     | Line { traceName "singleLine" [$1] }
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
