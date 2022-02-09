{
module Lexer where
import Control.Monad.State
}
%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$op = [!# \$ \% \+ \- \/ \* \. \< \> \= \? \@ \^ \| ]

tokens :-

  $white+				;
  "--".*				;
  let					{ tok (\p s -> Let p) }
  in					{ tok (\p s -> In p)}
  \=                    { tok (\p s -> Eq p) }
  \".*\"				{ tok (\p s -> Str p s) }
  \`                    { tok (\p s -> Backtick p) }
  $digit+ {tok (\p s -> Int p (read s))}
  $alpha($alpha|$digit)* {tok (\p s -> Identifier p s)}
  $op+ {tok (\p s -> Operator p s)}

{
tok f p s = f p s
data Token =
  Let AlexPosn
  | In AlexPosn
  | Eq AlexPosn
  | Backtick AlexPosn
  | Int AlexPosn Int
  | Str AlexPosn String
  | Identifier AlexPosn String
  | Operator AlexPosn String
  deriving (Show, Eq)

tokenPosition :: Token -> AlexPosn
tokenPosition (Let p) = p
tokenPosition (In p) = p
tokenPosition (Eq p) = p
tokenPosition (Int p _) = p
tokenPosition (Identifier p _) = p
tokenPosition (Operator p _) = p
tokenPosition (Str p _) = p
}
