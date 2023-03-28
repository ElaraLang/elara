{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Elara.Lexer.Utils where

import Codec.Binary.UTF8.String (encodeChar)
import Control.Lens (makeLenses, use, view)
import Data.List.NonEmpty (span, (<|))
import Data.Text qualified as T
import Data.Word (Word8)
import Debug.Trace (traceM, traceShowId, traceShowM)
import Elara.AST.Region (Located (Located), Position (RealPosition), RealPosition (..), RealSourceRegion (SourceRegion), SourceRegion (GeneratedRegion, RealSourceRegion))
import Elara.Lexer.Token (Lexeme, TokPosition, Token (TokenLeftBrace, TokenRightBrace, TokenRightParen, TokenSemicolon))
import Print (debugColored)
import Prelude hiding (span)

type P a = State ParseState a

data AlexInput = AlexInput
    { _filePath :: FilePath
    , _prev :: Char
    , _bytes :: [Word8]
    , _rest :: String
    , _lineNumber :: Int
    , _columnNumber :: Int
    }
    deriving (Show)

data ParseState = ParseState
    { _input :: AlexInput
    , _lexSC :: Int -- lexer start code
    , _stringBuf :: String -- temporary storage for strings
    , _pendingTokens :: [Lexeme] -- right now used when Parser consumes the lookeahead and decided to put it back
    , _indentStack :: NonEmpty Int -- stack of indentation levels
    , _pendingPosition :: TokPosition -- needed when parsing strings, chars, multi-line strings
    }
    deriving (Show)

makeLenses ''AlexInput
makeLenses ''ParseState

initialState :: FilePath -> String -> ParseState
initialState fp s =
    ParseState
        { _input =
            AlexInput
                { _filePath = fp
                , _prev = '\n'
                , _bytes = []
                , _rest = s
                , _lineNumber = 1
                , _columnNumber = 1
                }
        , _lexSC = 0
        , _stringBuf = ""
        , _pendingTokens = []
        , _indentStack = 0 :| []
        , _pendingPosition = Position 1 1
        }

fake :: Token -> P Lexeme
fake t = do
    fp <- use (input . filePath)
    pure (Located (GeneratedRegion fp) t)

startWhite :: Int -> Text -> P (Maybe Lexeme)
startWhite _ str = do
    let indentation = T.length $ T.dropWhile (== '\n') str
    s <- get
    let indents@(cur :| _) = _indentStack s
    case indentation `compare` cur of
        GT -> do
            fakeLb <- fake TokenLeftBrace
            put s{_indentStack = indentation <| indents, _pendingTokens = fakeLb : _pendingTokens s}
            pure Nothing
        LT -> do
            case span (> indentation) indents of
                (pre, top : xs) -> do
                    fakeClosings <- sequenceA [fake TokenRightBrace]
                    if top == indentation
                        then
                            put
                                s
                                    { _indentStack = top :| xs
                                    , _pendingTokens = pre >>= const fakeClosings
                                    }
                        else error $ "Indents don't match ( " <> show top <> " vs " <> show indentation <> ")" <> show s
                (_, []) -> error $ "Indents don't match ( [] vs " <> show indentation <> ")" <> show s
            pure Nothing
        EQ -> Just <$> fake TokenSemicolon

-- Insert } and ; tokens for any leftover unclosed indents
cleanIndentation :: P [Lexeme]
cleanIndentation = do
    indentStack <- use indentStack
    fakeClosings <- sequenceA [fake TokenRightBrace]
    modify $ \s -> s{_indentStack = 0 :| []}
    pure $ init indentStack >>= const fakeClosings

-- The functions that must be provided to Alex's basic interface

-- The input: last character, unused bytes, remaining string
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte ai@AlexInput{..} =
    case _bytes of
        (b : bs) ->
            Just (b, ai{_bytes = bs})
        [] ->
            case _rest of
                [] -> Nothing
                (char : chars) ->
                    let n = _lineNumber
                        n' = if char == '\n' then n + 1 else n
                        c = _columnNumber
                        c' = if char == '\n' then 1 else c + 1
                        (b :| bs) = fromList $ encodeChar char
                     in Just
                            ( b
                            , ai
                                { _prev = char
                                , _bytes = bs
                                , _rest = chars
                                , _lineNumber = n'
                                , _columnNumber = c'
                                }
                            )

getLineNo :: P Int
getLineNo = use (input . lineNumber)

getColNo :: P Int
getColNo = use (input . columnNumber)

evalP :: P a -> FilePath -> String -> a
evalP m fp s = evalState m (initialState fp s)

getPosition :: Int -> P TokPosition
getPosition tokenLength = do
    ParseState{_input = AlexInput{_lineNumber = ln, _columnNumber = cn}} <- get
    pure $ Position ln (cn - tokenLength)

createRegion :: TokPosition -> TokPosition -> P RealSourceRegion
createRegion start end = do
    fp <- use (input . filePath)
    pure $ SourceRegion (Just fp) start end

createRegionStartingAt :: TokPosition -> P RealSourceRegion
createRegionStartingAt start = do
    end <- getPosition 0
    createRegion start end
