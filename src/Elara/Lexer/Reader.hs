module Elara.Lexer.Reader where

import Control.Lens
import Elara.AST.Region (line, unlocated)
import Elara.Lexer.Lexer
import Elara.Lexer.Token
import Elara.Lexer.Utils
import Elara.ReadFile (readFileString)
import Polysemy
import Polysemy.Error (Error, throw)
import Polysemy.State

-- TODO: maybe also define empty Constructor for TokPosition
-- use it when constructing and here compute position and update it and return complete and correct Token
-- TODO2: I may actually not need to store position in the Token at all
-- thanks to monadic parsing I have the state at every moment of parsing --> just need to find out
-- how to use the state for better parse-error messages
readToken :: LexMonad Lexeme
readToken = do
  s <- get
  case s ^. pendingTokens of
    (token : tokens) -> do
      modify (pendingTokens .~ tokens)
      pure token
    [] ->
      case alexScan (s ^. input) (s ^. lexSC) of
        AlexEOF -> do
          when (s ^. lexSC == stringSC) (throw (UnterminatedStringLiteral s))
          eof <- fake TokenEOF
          closeIndents <- cleanIndentation
          modify (over pendingTokens (<> (closeIndents <> [eof])))
          readToken
        AlexError token -> error $ "Lexical error on line " <> show (token ^. position . line)
        AlexSkip inp _ -> do
          put s {_input = inp}
          readToken
        AlexToken inp n act -> do
          let buf = s ^. input . rest
          put s {_input = inp}
          res <- act n (toText (take n buf))
          maybe readToken pure res

readTokens :: LexMonad [Lexeme]
readTokens = do
  tok <- readToken
  case tok ^. unlocated of
    TokenEOF -> pure []
    _ -> do
      next <- readTokens
      pure (tok : next)

readTokensWith :: (Member (Error LexerError) r) => FilePath -> String -> Sem r [Lexeme]
readTokensWith fp s = do
  evalState (initialState fp s) (subsume_ readTokens)

lexer :: (Lexeme -> LexMonad a) -> LexMonad a
lexer cont = do
  tok <- readToken
  cont tok
