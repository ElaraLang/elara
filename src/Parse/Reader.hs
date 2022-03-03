module Parse.Reader where

import Control.Monad.State.Lazy
import Parse.Lexer
import Parse.Token
import Parse.Utils

-- TODO: maybe also define empty Constructor for TokPosition
-- use it when constructing and here compute position and update it and return complete and correct Token
-- TODO2: I may actually not need to store position in the Token at all
-- thanks to monadic parsing I have the state at every moment of parsing --> just need to find out
-- how to use the state for better parse-error messages
readToken :: P Token
readToken = do
  s <- get
  case pending_tokens s of
    (token : tokens) -> do
      put s {pending_tokens = tokens}
      return token
    [] ->
      case alexScan (input s) (lexSC s) of
        AlexEOF -> do
          put s {pending_tokens = [EOF]}
          readToken
        AlexError token -> error $ "Lexical error on line " ++ show (ai_line_number token)
        AlexSkip inp _ -> do
          put s {input = inp}
          readToken
        AlexToken inp n act -> do
          let AlexInput {ai_rest = buf} = input s -- TODO: rename airest
          put s {input = inp}
          res <- act n (take n buf)
          maybe readToken return res

readTokens :: P [Token]
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
