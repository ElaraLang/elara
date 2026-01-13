{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Elara.Lexer.Utils where

import Codec.Binary.UTF8.String (encodeChar)
import Data.Char
import Data.Kind (Type)
import Data.List.NonEmpty (span, (<|))
import Data.Text qualified as T
import Elara.AST.Name (ModuleName (..))
import Elara.AST.Region (Located (Located), RealPosition (..), RealSourceRegion (..), SourceRegion (..), column, line, mkSourceRegionIn, positionToDiagnosePosition)
import Elara.Error
import Elara.Error.Codes qualified as Codes
import Elara.Lexer.Token (Lexeme, TokPosition, Token (..), tokenEndsExpr)
import Error.Diagnose (Marker (..), Note (..), Report (Err))

import Effectful (Eff)
import Effectful.Error.Static
import Effectful.State.Extra (use')
import Effectful.State.Static.Local
import Elara.Logging (StructuredDebug)
import Prelude hiding (span)

data AlexInput = AlexInput
    { _filePath :: FilePath
    , _prev :: Char
    , _bytes :: [Word8]
    , _rest :: Text
    , _position :: RealPosition
    }
    deriving (Show)

data IndentInfo = IndentInfo
    { _indent :: Int
    , _indentPos :: RealPosition
    , _openedAtDepth :: Int
    -- ^ the delimDepth when this indent was opened
    }
    deriving (Show)

data ParseState = ParseState
    { _input :: AlexInput
    , _lexSC :: Int
    -- ^ lexer start code
    , _stringBuf :: Text
    -- ^ temporary storage for strings
    , _pendingTokens :: [Lexeme]
    -- ^ right now used when Parser consumes the lookahead and decided to put it back
    , _indentStack :: NonEmpty IndentInfo
    -- ^ stack of indentation levels
    , _pendingPosition :: TokPosition
    -- ^ needed when parsing strings, chars, multi-line strings
    , _prevEndsExpr :: Bool
    -- ^ did the previous token end an expression? (used for offside rule
    , _delimDepth :: Int
    -- ^ current depth of open delimiters ((), [], {
    , _commentDepth :: Int
    -- ^ nested comment depth
    }
    deriving (Show)

makeLenses ''AlexInput
makeLenses ''IndentInfo
makeLenses ''ParseState

type LexMonad :: Type -> Type
type LexMonad a = Eff '[State ParseState, Error LexerError, StructuredDebug] a

setPrevEndsExpr :: Bool -> LexMonad ()
setPrevEndsExpr b = modify (\st -> st{_prevEndsExpr = b})

getPrevEndsExpr :: LexMonad Bool
getPrevEndsExpr = gets _prevEndsExpr

mkIndentInfo :: Int -> LexMonad IndentInfo
mkIndentInfo i = do
    pos <- use' (input % position)
    d <- use' delimDepth
    pure (IndentInfo i pos d)

data LexerError
    = -- | When an element is indented more than expected
      TooMuchIndentation
        -- | The expected indentation
        IndentInfo
        -- | The potential further indentation
        (Maybe IndentInfo)
        -- | The actual indentation
        Int
        -- | The current state of the lexer
        ParseState
    | UnterminatedStringLiteral ParseState
    deriving (Show)

instance ReportableError LexerError where
    report (TooMuchIndentation expected further actual s) = do
        let fp = view (input % filePath) s
        let pos = view (input % position) s
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
        let fp = view (input % filePath) s
        let pos = view (input % position) s
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

initialState :: FilePath -> Text -> ParseState
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
        , _indentStack =
            IndentInfo
                0
                (Position 1 1)
                0
                :| []
        , _pendingPosition = Position 1 1
        , _prevEndsExpr = True
        , _delimDepth = 0
        , _commentDepth = 0
        }

pushFront :: Lexeme -> LexMonad ()
pushFront lex = modify (over pendingTokens (lex :))

pushBack :: Lexeme -> LexMonad ()
pushBack lex = modify (over pendingTokens (\toks -> toks <> [lex]))

-- Emits a token, updating the prevEndsExpr state if necessary
emitAt :: Token -> SourceRegion -> LexMonad (Maybe Lexeme)
emitAt t region = do
    prevEnds <- getPrevEndsExpr
    -- Suppress LINESEP if previous token cannot end an expression
    case t of
        TokenLeftParen; TokenLeftBracket; TokenLeftBrace -> modify (delimDepth %~ (+ 1)) *> emit
        TokenRightParen; TokenRightBracket; TokenRightBrace -> emitCloser t region
        TokenLineSeparator | not prevEnds -> do
            pure Nothing
        _ -> emit
  where
    emit = do
        setPrevEndsExpr (tokenEndsExpr t)
        pure (Just (Located region t))

popPending :: LexMonad (Maybe Lexeme)
popPending = do
    s <- get
    case _pendingTokens s of
        (x : xs) -> put s{_pendingTokens = xs} >> pure (Just x)
        [] -> pure Nothing

{- | Emit DEDENTs for layout opened inside current delimiter depth, then the closer.
Return the first pending token (typically a DEDENT) so stream becomes "… <DEDENT> ) …"
-}
emitCloser :: Token -> SourceRegion -> LexMonad (Maybe Lexeme)
emitCloser closerTok closerReg = do
    flushLayoutBeforeCloser
    modify (delimDepth %~ (\x -> max 0 (x - 1)))
    let closerLex = Located closerReg closerTok
    pushBack closerLex
    m <- popPending
    -- keep prevEndsExpr consistent
    case m of
        Just (Located _ tok) -> setPrevEndsExpr (tokenEndsExpr tok)
        Nothing -> setPrevEndsExpr (tokenEndsExpr closerTok)
    pure m

-- | Close only layout started at or inside current delimiter depth
flushLayoutBeforeCloser :: LexMonad ()
flushLayoutBeforeCloser = do
    d <- use' delimDepth
    st <- get
    let stk = st ^. indentStack
        (toClose, keep) = span (\ii -> ii ^. openedAtDepth >= d) stk
    case keep of
        [] -> pass -- base should remain
        (b : bs) -> do
            put (st & indentStack .~ (b :| bs))
            curPos <- use' (input % position)
            ds <- catMaybes <$> mapM (\lvl -> emitDedentAt (lvl ^. indentPos) curPos) toClose
            mapM_ pushFront (reverse ds)

emitLayoutAt :: Token -> RealSourceRegion -> LexMonad (Maybe Lexeme)
emitLayoutAt t r = pure (Just (Located (RealSourceRegion r) t))

emitIndentAt :: RealPosition -> LexMonad (Maybe Lexeme)
emitIndentAt pos = do
    r <- createRegion pos pos
    emitLayoutAt TokenIndent r

emitDedentAt :: TokPosition -> TokPosition -> LexMonad (Maybe Lexeme)
emitDedentAt start end = do
    r <- createRegion start end
    emitLayoutAt TokenDedent r

emitLineSepAt :: RealPosition -> LexMonad (Maybe Lexeme)
emitLineSepAt pos = do
    r <- createRegion pos pos
    emitLayoutAt TokenLineSeparator r

fake :: Token -> LexMonad (Maybe Lexeme)
fake t = do
    region <- getPosition 0 >>= createRegionStartingAt
    emitAt t (RealSourceRegion region)

startWhite :: Int -> Text -> LexMonad (Maybe Lexeme)
startWhite _ str = do
    let indentation = T.length $ T.dropWhile (== '\n') str
    s <- get
    let indents@(cur :| _) = s ^. indentStack

    curPos <- use' (input % position)
    case indentation `compare` (cur ^. indent) of
        GT -> do
            fakeLb <- emitIndentAt curPos
            indentInfo <- mkIndentInfo indentation
            let push = maybe identity (:) fakeLb
            -- Indent starts a block, usually shouldn't immediately end expr
            setPrevEndsExpr False
            put s{_indentStack = indentInfo <| indents, _pendingTokens = push (_pendingTokens s)}
            pure Nothing
        LT -> do
            -- If the indentation is less than the current indentation, we need to close the current block
            let (closingLevels, topAndRest) = span (view (indent % to (> indentation))) indents
            case topAndRest of
                (top : xs) -> do
                    eofPos <- use' (input % position)

                    dedents <- catMaybes <$> mapM (\lvl -> emitDedentAt (lvl ^. indentPos) eofPos) closingLevels
                    -- emit at most one layout line separator for this line
                    sep <- emitLineSepAt eofPos
                    setPrevEndsExpr False
                    let closings = dedents <> maybeToList sep
                    if top ^. indent == indentation
                        then
                            put
                                s
                                    { _indentStack = top :| xs
                                    , _pendingTokens = closings <> _pendingTokens s
                                    }
                        else throwError (TooMuchIndentation top (viaNonEmpty last $ init indents) indentation s)
                [] -> error (" Indent stack contains nothing greater than " <> show indentation)
            pure Nothing
        EQ -> do
            ends <- getPrevEndsExpr
            pos <- use' (input % position)
            if ends
                then do
                    setPrevEndsExpr False
                    emitLineSepAt pos
                else pure Nothing

-- Insert dedent for any leftover unclosed indents
cleanIndentation :: LexMonad [Lexeme]
cleanIndentation = do
    indentStack' <- use' indentStack
    case indentStack' of
        _base :| [] -> pure []
        _ -> do
            let toClose = init indentStack'
                base = last indentStack'
            eofPos <- use' (input % position)
            dedents <- catMaybes <$> mapM (\lvl -> emitDedentAt (lvl ^. indentPos) eofPos) toClose
            modify $ \s -> s{_indentStack = base :| []}
            pure dedents

-- The functions that must be provided to Alex's basic interface

-- The input: last character, unused bytes, remaining string
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte ai@AlexInput{..} =
    case _bytes of
        (b : bs) ->
            Just (b, ai{_bytes = bs})
        [] ->
            case T.uncons _rest of
                Nothing -> Nothing
                Just (char, chars) ->
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
getLineNo = use' (input % position % line)

getColNo :: LexMonad Int
getColNo = use' (input % position % column)

getPosition :: Int -> LexMonad TokPosition
getPosition tokenLength = do
    ParseState{_input = AlexInput{_position = (Position ln cn)}} <- get
    pure $ Position ln (cn - tokenLength)

createRegion :: TokPosition -> TokPosition -> LexMonad RealSourceRegion
createRegion start end = do
    fp <- use' (input % filePath)
    pure $ mkSourceRegionIn (Just fp) start end

createRegionStartingAt :: TokPosition -> LexMonad RealSourceRegion
createRegionStartingAt start = do
    end <- getPosition 0
    createRegion start end

{- | splits a qualified name into the qualifier and the name.
Throws an error if the name is not qualified.

Examples:

>>> splitQualName "Hello.world"
(ModuleName ("Hello" :| []),"world")

>>> splitQualName "A.B.C"
(ModuleName ("A" :| ["B"]),"C")

>>> splitQualName "Prelude..+"
(ModuleName ("Prelude" :| []),".+")

>>> splitQualName "A.!."
(ModuleName ("A" :| []),"!.")
-}
splitQualName :: Text -> (ModuleName, Text)
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
            -- >>> ["A", "!", ""] = ("A", "!.")
            let isAlphaNumeric = T.all (\c -> isAlpha c || isDigit c)
                (modPart, namePart) = span (liftA2 (&&) isAlphaNumeric (not . T.null)) (fromList xs)
             in if null namePart
                    then -- TODO: this isn't very efficient
                        (ModuleName $ fromList (init (fromList modPart)), last (fromList modPart))
                    else (ModuleName $ fromList modPart, T.intercalate "." namePart)
