module Elara.Lexer.Action where

import Data.Text qualified as Text
import Effectful.State.Static.Local
import Elara.AST.Region (Located (..), SourceRegion (RealSourceRegion))
import Elara.Lexer.Token
import Elara.Lexer.Utils

type NumberOfCharsMatched = Int
type MatchedSequence = Text
type LexAction = NumberOfCharsMatched -> MatchedSequence -> LexMonad (Maybe Lexeme)

simpleTok :: Token -> LexAction
simpleTok t len _ = do
    start <- getPosition len
    region <- createRegionStartingAt start
    pure $ Just (Located (RealSourceRegion region) t)

parametrizedTok :: (a -> Token) -> (Text -> a) -> LexAction
parametrizedTok tc read' tokenLen matched = do
    start <- getPosition tokenLen
    region <- createRegionStartingAt start
    let token = tc (read' matched)
    pure $ Just (Located (RealSourceRegion region) token)

beginString :: _ -> LexAction
beginString stringSC len _ = do
    s <- get
    pos <- getPosition len
    put s{_lexSC = stringSC, _pendingPosition = pos}
    pure Nothing

appendToString :: LexAction
appendToString = appendToStringWith Text.head

appendToStringWith :: (Text -> Char) -> LexAction
appendToStringWith f len inp = do
    modify $ over stringBuf (f (Text.take len inp) `Text.cons`)
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
            { _lexSC = 0
            , _stringBuf = ""
            }
    let token = TokenString (Text.reverse buf)
    pure $ Just (Located (RealSourceRegion region) token)
