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

-- TODO: maybe also define empty Constructor for TokPosition
-- use it when constructing and here compute position and update it and return complete and correct Token
-- TODO2: I may actually not need to store position in the Token at all
-- thanks to monadic parsing I have the state at every moment of parsing --> just need to find out
-- how to use the state for better parse-error messages
readToken :: P Token
readToken = do
  s <- get
  case pending'tokens s of
    (tok : toks) -> do
      put s{ pending'tokens = toks }
      return tok
    [] ->
      case alexScan (input s) (lexSC s) of
        AlexEOF -> do
                     r <- startWhite 1 ""
                     put s {pending'tokens = pending'tokens s ++ [EOF]}
                     case r of
                        Nothing -> readToken
                        Just rval -> return rval
        AlexError inp' -> error $ "Lexical error on line " ++ (show $ ai_line_number inp')

        AlexSkip inp' _ -> do
          put s{ input = inp' }
          readToken

        AlexToken inp' n act -> do
          -- let ll = layout'stack s
          let (AlexInput{ ai_rest = buf }) = input s -- TODO: rename airest
          put s{ input = inp' }
          res <- act n (take n buf)
          case res of
            Nothing -> readToken
            Just t -> return t


readTokens = do
  tok <- readToken
  case tok of
    EOF -> return []
    _ -> do
      rest <- readTokens
      return (tok : rest)


lexer :: (Token -> P a) -> P a
lexer cont = do
  tok <- readToken
  cont tok
}
