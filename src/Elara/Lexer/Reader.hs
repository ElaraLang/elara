module Elara.Lexer.Reader where

import Elara.AST.Region (line, unlocated)
import Elara.Lexer.Lexer
import Elara.Lexer.Token
import Elara.Lexer.Utils

import Data.Text qualified as Text
import Effectful (Eff, inject, (:>))
import Effectful.Error.Static
import Effectful.FileSystem (FileSystem)
import Effectful.State.Static.Local
import Elara.Query (Query (GetFileContents))
import Elara.ReadFile (FileContents (FileContents))
import Rock qualified

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
                    when (s ^. lexSC == stringSC) (throwError (UnterminatedStringLiteral s))
                    eof <- fake TokenEOF
                    closeIndents <- cleanIndentation
                    modify (over pendingTokens (<> (closeIndents <> [eof])))
                    readToken
                AlexError token -> error $ "Lexical error on line " <> show (token ^. position % line)
                AlexSkip inp _ -> do
                    put s{_input = inp}
                    readToken
                AlexToken inp n act -> do
                    let buf = s ^. input % rest
                    put s{_input = inp}
                    res <- act n (Text.take n buf)
                    maybe readToken pure res

readTokens :: LexMonad [Lexeme]
readTokens = do
    tok <- readToken
    case tok ^. unlocated of
        TokenEOF -> pure []
        _ -> do
            next <- readTokens
            pure (tok : next)

readTokensWith :: Error LexerError :> es => FileContents -> Eff es [Lexeme]
readTokensWith (FileContents fp s) = do
    evalState (initialState fp s) (inject readTokens)

getLexedFile :: FilePath -> Eff '[FileSystem, Rock.Rock Query, Error LexerError] [Lexeme]
getLexedFile fp = do
    fileContents <- Rock.fetch (GetFileContents fp)
    readTokensWith fileContents

lexer :: (Lexeme -> LexMonad a) -> LexMonad a
lexer cont = do
    tok <- readToken
    cont tok
