module Elara.Lexer.Action where

import Data.Text qualified as Text
import Effectful.State.Static.Local
import Elara.AST.Region (SourceRegion (RealSourceRegion))
import Elara.Lexer.Token
import Elara.Lexer.Utils

type NumberOfCharsMatched = Int
type MatchedSequence = Text
type LexAction = NumberOfCharsMatched -> MatchedSequence -> LexMonad (Maybe Lexeme)

simpleTok :: Token -> LexAction
simpleTok t len _ = do
    start <- getPosition len
    region <- createRegionStartingAt start
    emitAt t (RealSourceRegion region)

parametrizedTok :: (a -> Token) -> (Text -> a) -> LexAction
parametrizedTok tc read' tokenLen matched = do
    start <- getPosition tokenLen
    region <- createRegionStartingAt start
    let token = tc (read' matched)
    emitAt token (RealSourceRegion region)

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
    emitAt token (RealSourceRegion region)

-- | Single line comments just consume everything
singleLineComment :: LexAction
singleLineComment _ _ = pure Nothing

beginBlockComment :: Int -> LexAction
beginBlockComment commentSC len _ = do
    modify $ \s -> s{_commentDepth = 1, _lexSC = commentSC}
    pure Nothing

-- | End block comment: decrement depth, return to normal when it reaches 0
endBlockComment :: LexAction
endBlockComment _ _ = do
    modify $ \s -> s{_commentDepth = _commentDepth s - 1}
    s <- get
    when (_commentDepth s == 0) $
        modify $
            \st -> st{_lexSC = 0}
    pure Nothing

-- | Nested block comment: increment depth
nestedBlockComment :: LexAction
nestedBlockComment _ _ = do
    modify $ \s -> s{_commentDepth = _commentDepth s + 1}
    pure Nothing

-- | Consume any character inside block comment
commentChar :: LexAction
commentChar _ _ = pure Nothing
