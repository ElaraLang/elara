{-# LANGUAGE LexicalNegation #-}

module Elara.Lexer.Action where

import Data.Text qualified as Text
import Effectful.State.Extra (modifying, (.=))
import Effectful.State.Static.Local
import Elara.AST.Region (SourceRegion (RealSourceRegion))
import Elara.Lexer.Token
import Elara.Lexer.Utils

type NumberOfCharsMatched = Int
type MatchedSequence = Text
type LexAction = NumberOfCharsMatched -> MatchedSequence -> LexMonad (Maybe Lexeme)

simpleTok :: Token -> LexAction
simpleTok t len _ = do
    checkBlockLayout len
    start <- getPosition len
    region <- createRegionStartingAt start
    emitAt t (RealSourceRegion region)

inlineLayoutTok :: Token -> LexAction
inlineLayoutTok t len seq = do
    res <- simpleTok t len seq
    triggerBlockLayout
    pure res

indentLayoutTok :: Token -> LexAction
indentLayoutTok t len seq = do
    res <- simpleTok t len seq
    triggerIndentLayout
    pure res

parametrizedTok :: (a -> Token) -> (Text -> a) -> LexAction
parametrizedTok tc read' tokenLen matched = do
    checkBlockLayout tokenLen
    start <- getPosition tokenLen
    region <- createRegionStartingAt start
    let token = tc (read' matched)
    emitAt token (RealSourceRegion region)

beginString :: _ -> LexAction
beginString stringSC len _ = do
    pos <- getPosition len
    lexSC .= stringSC
    pendingPosition .= pos
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
    lexSC .= 0
    stringBuf .= ""
    let token = TokenString (Text.reverse buf)
    emitAt token (RealSourceRegion region)

-- | Single line comments just consume everything
singleLineComment :: LexAction
singleLineComment _ _ = pure Nothing

beginBlockComment :: Int -> LexAction
beginBlockComment commentSC _len _ = do
    lexSC .= commentSC
    modifying commentDepth (+ 1)
    pure Nothing

-- | End block comment: decrement depth, return to normal when it reaches 0
endBlockComment :: LexAction
endBlockComment _ _ = do
    modifying commentDepth (- 1)
    s <- get
    when (s ^. commentDepth == 0) $
        lexSC .= 0
    pure Nothing

-- | Nested block comment: increment depth
nestedBlockComment :: LexAction
nestedBlockComment _ _ = do
    modifying commentDepth (+ 1)
    pure Nothing

-- | Consume any character inside block comment
commentChar :: LexAction
commentChar _ _ = pure Nothing
