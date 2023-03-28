{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Elara.Lexer.Utils where

import Codec.Binary.UTF8.String (encodeChar)
import Control.Lens (makeLenses, use, view, (^.))
import Data.List.NonEmpty (span, (<|))
import Data.Text qualified as T
import Data.Word (Word8)
import Debug.Trace (traceM, traceShowId, traceShowM)
import Elara.AST.Region (Located (Located), Position (RealPosition), RealPosition (..), RealSourceRegion (SourceRegion), SourceRegion (GeneratedRegion, RealSourceRegion), column, line, positionToDiagnosePosition)
import Elara.Error
import Elara.Error.Codes qualified as Codes
import Elara.Lexer.Token (Lexeme, TokPosition, Token (TokenLeftBrace, TokenRightBrace, TokenRightParen, TokenSemicolon))
import Error.Diagnose (Marker (..), Note (..), Report (Err))
import Polysemy
import Polysemy.Error
import Polysemy.MTL
import Polysemy.State
import Print (debugColored)
import Prelude hiding (State, evalState, get, modify, put, span)

data AlexInput = AlexInput
    { _filePath :: FilePath
    , _prev :: Char
    , _bytes :: [Word8]
    , _rest :: String
    , _position :: RealPosition
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

type LexMonad :: Type -> Type
type LexMonad a = Sem [State ParseState, Error LexerError] a

data LexerError
    = -- | When an element is indented more than expected
      TooMuchIndentation
        Int
        -- ^ The expected indentation
        (Maybe Int)
        -- ^ The potential further indentation
        Int
        -- ^ The actual indentation
        ParseState
        -- ^ The current state of the lexer
    deriving (Show)

instance ReportableError LexerError where
    report (TooMuchIndentation expected further actual s) = do
        let fp = view (input . filePath) s
        let pos = view (input . position) s
        let msg = "Unexpected indentation. Expected " <> show expected <> " spaces, but got " <> show actual <> " spaces."
        let hint = case further of
                Nothing -> "Try removing the extra indentation."
                Just f -> "Try removing the extra indentation or indenting the line by " <> show f <> " spaces."
        writeReport $
            Err
                (Just Codes.tooMuchIndentation)
                msg
                [(positionToDiagnosePosition fp pos, This "this line is indented too far")]
                [Note "When using lightweight syntax, the level of indentation is very important. Currently, I can't tell what expression this line is supposed to be a part of. ", Hint hint]

initialState :: FilePath -> String -> ParseState
initialState fp s =
    ParseState
        { _input =
            AlexInput
                { _filePath = fp
                , _prev = '\n'
                , _bytes = []
                , _rest = s
                , _position = Position 1 1
                }
        , _lexSC = 0
        , _stringBuf = ""
        , _pendingTokens = []
        , _indentStack = 0 :| []
        , _pendingPosition = Position 1 1
        }

evalLexMonad :: FilePath -> String -> LexMonad a -> Either LexerError a
evalLexMonad fp s = run . runError . evalState (initialState fp s)

fake :: Token -> LexMonad Lexeme
fake t = do
    fp <- use (input . filePath)
    pure (Located (GeneratedRegion fp) t)

startWhite :: Int -> Text -> LexMonad (Maybe Lexeme)
startWhite _ str = do
    let indentation = T.length $ T.dropWhile (== '\n') str
    s <- get
    let indents@(cur :| _) = s ^. indentStack
    case indentation `compare` cur of
        GT -> do
            fakeLb <- fake TokenLeftBrace
            put s{_indentStack = indentation <| indents, _pendingTokens = fakeLb : _pendingTokens s}
            pure Nothing
        LT -> do
            -- If the indentation is less than the current indentation, we need to close the current block
            case span (> indentation) indents of
                (pre, top : xs) -> do
                    -- pre is all the levels that need to be closed, top is the level that we need to match
                    fakeClosings <- sequenceA [fake TokenRightBrace, fake TokenSemicolon]
                    if top == indentation
                        then
                            put
                                s
                                    { _indentStack = top :| xs
                                    , _pendingTokens = pre >>= const fakeClosings
                                    }
                        else throw (TooMuchIndentation top (viaNonEmpty last $ init indents) indentation s)
                (_, []) -> error (" Indent stack contains nothing greater than " <> show indentation)
            pure Nothing
        EQ -> Just <$> fake TokenSemicolon

-- Insert }  for any leftover unclosed indents
cleanIndentation :: LexMonad [Lexeme]
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
                    let (Position n c) = _position
                        n' = if char == '\n' then n + 1 else n
                        c' = if char == '\n' then 1 else c + 1
                        (b :| bs) = fromList $ encodeChar char
                     in Just
                            ( b
                            , ai
                                { _prev = char
                                , _bytes = bs
                                , _rest = chars
                                , _position = Position n' c'
                                }
                            )

getLineNo :: LexMonad Int
getLineNo = use (input . position . line)

getColNo :: LexMonad Int
getColNo = use (input . position . column)

getPosition :: Int -> LexMonad TokPosition
getPosition tokenLength = do
    ParseState{_input = AlexInput{_position = (Position ln cn)}} <- get
    pure $ Position ln (cn - tokenLength)

createRegion :: TokPosition -> TokPosition -> LexMonad RealSourceRegion
createRegion start end = do
    fp <- use (input . filePath)
    pure $ SourceRegion (Just fp) start end

createRegionStartingAt :: TokPosition -> LexMonad RealSourceRegion
createRegionStartingAt start = do
    end <- getPosition 0
    createRegion start end
