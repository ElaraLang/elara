module Elara.Lexer.Reader where

import Elara.AST.Region (Located (Located), SourceRegion (..), unlocated)
import Elara.Lexer.Lexer
import Elara.Lexer.Token
import Elara.Lexer.Utils

import Data.Text qualified as Text
import Effectful (Eff, inject, (:>))
import Effectful.Error.Static
import Effectful.State.Extra (use', (.=))
import Effectful.State.Static.Local
import Elara.Error (runErrorOrReport)
import Elara.Logging (StructuredDebug)
import Elara.Query (Query (GetFileContents))
import Elara.Query.Effects
import Elara.ReadFile (FileContents (FileContents))
import Rock qualified

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
                    closeIndents <- cleanIndentation
                    pos <- use' (input % position)
                    region <- createRegion pos pos
                    let eofToken = Located (RealSourceRegion region) TokenEOF
                    modify (over pendingTokens (<> (closeIndents <> [eofToken])))
                    readToken
                AlexError token -> throwError (GenericAlexError token)
                AlexSkip inp _ -> do
                    input .= inp

                    readToken
                AlexToken inp n act -> do
                    let buf = s ^. input % rest
                    input .= inp

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

readTokensWith ::
    (Error LexerError :> es, StructuredDebug :> es) =>
    FileContents -> Eff es [Lexeme]
readTokensWith (FileContents fp s) = do
    evalState (initialLexState fp s) (inject readTokens)

getLexedFile :: FilePath -> Eff (ConsQueryEffects '[Error LexerError, Rock.Rock Query]) [Lexeme]
getLexedFile fp = do
    fileContents <- runErrorOrReport $ Rock.fetch (GetFileContents fp)
    readTokensWith fileContents

lexer :: (Lexeme -> LexMonad a) -> LexMonad a
lexer cont = do
    tok <- readToken
    cont tok
