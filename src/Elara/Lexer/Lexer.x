{
module Elara.Lexer.Lexer where
import Elara.Lexer.Token
import Elara.Lexer.Utils
import Relude.Unsafe (read)
import Control.Lens ((^.), over)
import Elara.AST.Region
import Data.Text qualified as T
}

$digit = 0-9
$white_no_nl = $white # \n 

$lower = [a-z]
$upper = [A-Z]
$op = [!# \$ \% \+ \- \/ \* \. \< \> \= \? \@ \^ \| \$ \& \~]
$identifier = [$lower $upper $digit]

@variableIdentifer = $lower $identifier*
@typeIdentifier = $upper $identifier*

tokens :-
  <0> \;                     { simpleTok TokenSemicolon }
  <0> \n$white_no_nl*        { startWhite }
  <0> $white+                ;
  <0> "--".*				         ;
  <0> let					           { simpleTok TokenLet }
  <0> def                    { simpleTok TokenDef }
  <0> if                     { simpleTok TokenIf }
  <0> then                   { simpleTok TokenThen }
  <0> else                   { simpleTok TokenElse }
  <0> in					           { simpleTok TokenIn }
  <0> match                  { simpleTok TokenMatch }
  <0> with                   { simpleTok TokenWith }
  <0> \-\>                   { simpleTok TokenRightArrow }
  <0> \=                     { simpleTok TokenEquals }
  <0> \`                     { simpleTok TokenBacktick }
  <0> :                      { simpleTok TokenColon }
  <0> ::                     { simpleTok TokenDoubleColon }
  <0> _                      { simpleTok TokenUnderscore }
  <0> \(                     { simpleTok TokenLeftParen }
  <0> \)                     { simpleTok TokenRightParen }
  <0> \[                     { simpleTok TokenLeftBracket }
  <0> \]                     { simpleTok TokenRightBracket }
  <0> \,                     { simpleTok TokenComma }
  <0> $digit+                { parametrizedTok TokenInt (read . toString) }
  <0> @variableIdentifer     { parametrizedTok TokenVariableIdentifier id }
  <0> @typeIdentifier        { parametrizedTok TokenConstructorIdentifier id }
  <0> $op+                   { parametrizedTok TokenOperatorIdentifier id }
  <0> \"                     { beginString }
  <stringSC> \"              { endString }
  <stringSC> .               { appendToString }
{

type NumberOfCharsMatched = Int
type MatchedSequence = Text
type LexAction = NumberOfCharsMatched -> MatchedSequence -> P (Maybe Lexeme)

simpleTok :: Token -> LexAction
simpleTok t len _ = do
  start <- getPosition len
  region <- createRegionStartingAt start 
  return $ Just (Located (RealSourceRegion region) t)

parametrizedTok :: (a -> Token) -> (Text -> a) -> LexAction
parametrizedTok tc read' tokenLen matched = do
  start <- getPosition tokenLen 
  region <- createRegionStartingAt start 
  let token = tc (read' matched)
  return $ Just (Located (RealSourceRegion region) token)


beginString :: LexAction
beginString len _ = do
  s <- get
  pos <- getPosition len
  put s{ _lexSC = stringSC, _pendingPosition = pos }
  pure Nothing

appendToString :: LexAction
appendToString _ inp = do
  modify $ over stringBuf (T.head inp :)
  pure Nothing

endString :: LexAction
endString len _ = do
  s <- get
  endPos <- getPosition len
  let buf = s ^. stringBuf
  let startPos = s ^. pendingPosition
  region <- createRegion startPos endPos
  put
    s
      { _lexSC = 0,
        _stringBuf = ""
      }
  let token = TokenString (toText $ reverse buf)
  pure $ Just (Located (RealSourceRegion region) token)
}