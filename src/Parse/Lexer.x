{
module Parse.Lexer where
}

%wrapper "monad"

$digit = 0-9
$alpha = [a-zA-Z]
$op = [!# \$ \% \+ \- \/ \* \. \< \> \= \? \@ \^ \| ]
$nl = [\n\r\f]
$whitechar = [$nl\v\ ]
$whitespace_no_newline = $whitechar # \n
$tab = \t


tokens :-

  $whitechar+ ;
  "--".*				;
  let					 { mkL QLet }
  in					 { mkL QIn}
  \=                     { mkL QEq }
  \".*\"				 { mkL QStr }
  \`                     { mkL QBacktick }
  $digit+                { mkL QInt }
  $alpha($alpha|$digit)* { mkL QIdentifier }
  $op+                   { mkL QOperator }

{

data LexemeClass =
  QLet
  | QIn
  | QEq
  | QBacktick
  | QInt
  | QStr
  | QIdentifier
  | QOperator
  deriving (Show, Eq)

data Token = Let AlexPosn
            | In AlexPosn
            | Eq AlexPosn
            | Backtick AlexPosn
            | Int AlexPosn Int
            | Str AlexPosn String
            | Identifier AlexPosn String
            | Operator AlexPosn String
            | EOF
            deriving (Show, Eq)

mkL :: LexemeClass -> AlexInput -> Int -> Alex Token
mkL c (p,_,_,str) len = let t = take len str in
                        return $ case c of
                          QLet -> Let p
                          QIn -> In p
                          QEq -> Eq p
                          QBacktick -> Backtick p
                          QInt -> Int p (read t)
                          QStr -> Str p t
                          QIdentifier -> Identifier p t
                          QOperator -> Operator p t


alexEOF = return EOF

showPosn (AlexPn _ line col) = show line ++ ":" ++ show col
}
