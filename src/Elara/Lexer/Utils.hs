{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Elara.Lexer.Utils where

import Codec.Binary.UTF8.String (encodeChar)
import Control.Lens (makeLenses, to, view, (^.))
import Data.Kind (Type)
import Data.List.NonEmpty (span, (<|))
import Data.Text qualified as T
import Elara.AST.Name (ModuleName (..))
import Elara.AST.Region (Located (Located), RealPosition (..), RealSourceRegion (..), SourceRegion (GeneratedRegion), column, line, positionToDiagnosePosition)
import Elara.Error
import Elara.Error.Codes qualified as Codes
import Elara.Lexer.Token (Lexeme, TokPosition, Token (TokenDedent, TokenIndent, TokenSemicolon))
import Elara.Pipeline (PipelineResultEff)
import Elara.ReadFile (readFileString)
import Error.Diagnose (Marker (..), Note (..), Report (Err))
import Polysemy
import Polysemy.Error
import Polysemy.State
import Polysemy.State.Extra
import Prelude hiding (span)

data AlexInput = AlexInput
    { _filePath :: FilePath
    , _prev :: Char
    , _bytes :: [Word8]
    , _rest :: String
    , _position :: RealPosition
    }
    deriving (Show)

data IndentInfo = IndentInfo
    { _indent :: Int
    , _indentPos :: RealPosition
    }
    deriving (Show)

data ParseState = ParseState
    { _input :: AlexInput
    , _lexSC :: Int -- lexer start code
    , _stringBuf :: String -- temporary storage for strings
    , _pendingTokens :: [Lexeme] -- right now used when Parser consumes the lookeahead and decided to put it back
    , _indentStack :: NonEmpty IndentInfo -- stack of indentation levels
    , _pendingPosition :: TokPosition -- needed when parsing strings, chars, multi-line strings
    }
    deriving (Show)

makeLenses ''AlexInput
makeLenses ''IndentInfo
makeLenses ''ParseState

type LexMonad :: Type -> Type
type LexMonad a = Sem '[State ParseState, Error LexerError] a

mkIndentInfo :: Int -> LexMonad IndentInfo
mkIndentInfo i = do
    pos <- use' (input . position)
    pure (IndentInfo i pos)

data LexerError
    = -- | When an element is indented more than expected
      TooMuchIndentation
        IndentInfo
        -- ^ The expected indentation
        (Maybe IndentInfo)
        -- ^ The potential further indentation
        Int
        -- ^ The actual indentation
        ParseState
        -- ^ The current state of the lexer
    | UnterminatedStringLiteral ParseState
    deriving (Show)

instance ReportableError LexerError where
    report (TooMuchIndentation expected further actual s) = do
        let fp = view (input . filePath) s
        let pos = view (input . position) s
        let msg = "Unexpected change in indentation. Expected " <> show (expected ^. indent) <> " spaces, but got " <> show actual <> " spaces."
        let hint = case further of
                Nothing -> "Try removing the extra indentation."
                Just f -> "Try removing the extra indentation or indenting the line by " <> show (f ^. indent - actual) <> " space(s)."

        let baseHints =
                [ (positionToDiagnosePosition fp pos, This "this line is indented incorrectly")
                , (positionToDiagnosePosition fp (expected ^. indentPos), Maybe "If the decrease in indentation is intentional, the problematic line should line up with this line.")
                ]
        let furtherHints = case further of
                Nothing -> baseHints
                Just f -> (positionToDiagnosePosition fp (f ^. indentPos), Maybe ("an offside rule begins here (column " <> show (f ^. indent) <> "). If you think the problematic line is \"related\" to this line, make sure they line up.")) : baseHints
        writeReport $
            Err
                (Just Codes.tooMuchIndentation)
                msg
                furtherHints
                [ Note "When using lightweight syntax, the level of indentation is very important. Currently, I can't tell what expression this line is supposed to be a part of as it doesn't line up with anything, and didn't appear in a place where indentation can begin."
                , Hint hint
                ]
    report (UnterminatedStringLiteral s) = do
        let fp = view (input . filePath) s
        let pos = view (input . position) s
        let msg = "Unterminated string literal."
        let hint = "Make sure that the string literal is terminated with a double quote (\")."
        writeReport $
            Err
                (Just Codes.unterminatedStringLiteral)
                msg
                [(positionToDiagnosePosition fp pos, This "this string literal is unterminated")]
                [ Note "String literals are delimited by double quotes (\")."
                , Hint hint
                ]

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
        , _indentStack = IndentInfo 0 (Position 1 1) :| []
        , _pendingPosition = Position 1 1
        }

fake :: Token -> LexMonad Lexeme
fake t = do
    fp <- use' (input . filePath)
    pure (Located (GeneratedRegion fp) t)

startWhite :: Int -> Text -> LexMonad (Maybe Lexeme)
startWhite _ str = do
    let indentation = T.length $ T.dropWhile (== '\n') str
    s <- get
    let indents@(cur :| _) = s ^. indentStack
    case indentation `compare` (cur ^. indent) of
        GT -> do
            fakeLb <- fake TokenIndent
            indentInfo <- mkIndentInfo indentation
            put s{_indentStack = indentInfo <| indents, _pendingTokens = fakeLb : _pendingTokens s}
            pure Nothing
        LT -> do
            -- If the indentation is less than the current indentation, we need to close the current block
            case span (view (indent . to (> indentation))) indents of
                (pre, top : xs) -> do
                    -- pre is all the levels that need to be closed, top is the level that we need to match
                    fakeClosings <- sequenceA [fake TokenDedent, fake TokenSemicolon]
                    if top ^. indent == indentation
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

-- Insert dedent for any leftover unclosed indents
cleanIndentation :: LexMonad [Lexeme]
cleanIndentation = do
    indentStack' <- use' indentStack
    fakeClosings <- sequenceA [fake TokenDedent]
    modify $ \s -> s{_indentStack = IndentInfo 0 (Position 1 1) :| []}
    pure $ init indentStack' >>= const fakeClosings

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
getLineNo = use' (input . position . line)

getColNo :: LexMonad Int
getColNo = use' (input . position . column)

getPosition :: Int -> LexMonad TokPosition
getPosition tokenLength = do
    ParseState{_input = AlexInput{_position = (Position ln cn)}} <- get
    pure $ Position ln (cn - tokenLength)

createRegion :: TokPosition -> TokPosition -> LexMonad RealSourceRegion
createRegion start end = do
    fp <- use' (input . filePath)
    pure $ SourceRegion (Just fp) start end

createRegionStartingAt :: TokPosition -> LexMonad RealSourceRegion
createRegionStartingAt start = do
    end <- getPosition 0
    createRegion start end

{- | splits a qualified name into the qualifier and the name

 Examples:

>>> splitQualName "Hello.world"
(ModuleName ("Hello" :| []),"world")

>>> splitQualName "A.B.C"
(ModuleName ("A" :| ["B"]),"C")

>>> splitQualName "A"
*** Exception: No module name

>>> splitQualName ""
*** Exception: Empty string

>>> splitQualName "Prelude..+"
(ModuleName ("Prelude" :| []),".+")
-}
splitQualName :: HasCallStack => Text -> (ModuleName, Text)
splitQualName t = do
    let parts = T.splitOn "." t
    case parts of
        [] -> error "Empty string"
        [""] -> error "Empty string"
        [_] -> error "No module name"
        xs ->
            -- we have to be careful here, we can't just take the 'init' because that will break operators that start with .
            -- >>> ["Prelude", "", "+"] = ("Prelude", ".+")
            -- >>> ["Prelude", "T"] = ("Prelude", "T")
            -- >>> ["Prelude", "T", ""] = ("Prelude.T", ".")
            let
                (modPart, namePart) = span (not . T.null) (fromList xs)
             in
                if null namePart
                    then -- TODO: this isn't very efficient
                        (ModuleName $ fromList (init (fromList modPart)), last (fromList modPart))
                    else (ModuleName $ fromList modPart, T.intercalate "." namePart)
