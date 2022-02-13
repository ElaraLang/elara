{
module Parse.Lexer where
import Parse.Token
import Parse.Utils
import Control.Monad.State.Lazy
}

$digit = 0-9

$lower = [a-z]
$upper = [A-Z]
$op = [!# \$ \% \+ \- \/ \* \. \< \> \= \? \@ \^ \| ]
$identifier = [$lower $upper $digit]

@variableIdentifer = $lower $identifier*

tokens :-
  <0> \;                     { simpleTok SemiColon }
  <0> \n$white*              { startWhite }
  <0> $white+                ;
  <0> "--".*				 ;
  <0> let					 { simpleTok Let }
  <0> if                     { simpleTok If }
  <0> then                   { simpleTok Then }
  <0> else                   { simpleTok Else }
  <0> in					 { simpleTok In}
  <0> \=                     { simpleTok Eq }
  <0> \`                     { simpleTok Backtick }
  <0> \[                     { simpleTok LSParen }
  <0> \]                     { simpleTok RSParen }
  <0> \,                     { simpleTok Comma }
  <0> $digit+                { parametrizedTok Int read }
  <0> @variableIdentifer { parametrizedTok Identifier id }
  <0> $op+                   { parametrizedTok Operator id }
  <0> \"                 { beginString }
  <stringSC> \"          { endString }
  <stringSC> .      { appendToString }
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

beginString :: LexAction
beginString len _ = do
  s <- get
  pos <- getPosition len
  put s{ lexSC = stringSC, pending_position = pos }
  return Nothing

appendToString :: LexAction
appendToString _ (c : _) = do
  s <- get
  put s{ stringBuf = c : (stringBuf s) }
  return Nothing

endString :: LexAction
endString _ _ = do
  s <- get
  let buf = stringBuf s
  put
    s
      { lexSC = 0,
        stringBuf = ""
      }
  let token = Str (reverse buf)
  return $ Just token
}
