{
module Parse.Lexer where
import Parse.Token
import Parse.Utils
import Control.Monad.State.Lazy
}

$digit = 0-9
$alpha = [a-zA-Z]
$op = [!# \$ \% \+ \- \/ \* \. \< \> \= \? \@ \^ \| ]


tokens :-
  \;                    { simpleTok SemiColon }
  \n$white*              { startWhite }
  $white+               ;
  "--".*				;
  let					 { simpleTok Let }
  if                    { simpleTok If }
  then                  { simpleTok Then }
  else                  { simpleTok Else }
  in					 { simpleTok In}
  \=                     { simpleTok Eq }
  \".*\"				 { parametrizedTok Str id }
  \`                     { simpleTok Backtick }
  \[                     { simpleTok LSParen }
  \]                     { simpleTok RSParen }
  \,                     { simpleTok Comma }
  $digit+                { parametrizedTok Int read }
  $alpha($alpha|$digit)* { parametrizedTok Identifier id }
  $op+                   { parametrizedTok Operator id }

{
type NumberOfCharsMatched = Int
type MatchedSequence = String
type LexAction = NumberOfCharsMatched -> MatchedSequence -> P (Maybe Token)

simpleTok :: Token -> LexAction
simpleTok t _ _ = return (Just t)

parametrizedTok :: (a -> Token) -> (String -> a) -> LexAction
parametrizedTok tc read' _ matched = do
  let token = tc (read' matched)
  return $ Just token

}
