module Parse.Utils where

import Codec.Binary.UTF8.String (encode)
import Control.Monad.State.Lazy
import Data.Word (Word8)
import Parse.Token

type P a = State ParseState a

data AlexInput = AlexInput
  { ai_prev :: Char,
    ai_bytes :: [Word8],
    ai_rest :: String,
    ai_line_number :: Int,
    ai_column_number :: Int
  }
  deriving (Show)

data ParseState = ParseState
  { input :: AlexInput,
    lexSC :: Int, -- lexer start code
    stringBuf :: String, -- temporary storage for strings
    pending_tokens :: [Token], -- right now used when Parser consumes the lookeahead and decided to put it back
    indent_stack :: [Int], -- stack of indentation levels
    pending_position :: TokPosition -- needed when parsing strings, chars, multi-line strings
  }
  deriving (Show)

initialState :: String -> ParseState
initialState s =
  ParseState
    { input =
        AlexInput
          { ai_prev = '\n',
            ai_bytes = [],
            ai_rest = s,
            ai_line_number = 1,
            ai_column_number = 1
          },
      lexSC = 0,
      stringBuf = "",
      pending_tokens = [],
      indent_stack = [0],
      pending_position = TokPosition {line = 1, column = 1}
    }

startWhite :: Int -> String -> P (Maybe Token)
startWhite _ str = do
  let n = length $ dropWhile (== '\n') str
  s <- get
  let indents@(cur : _) = indent_stack s
  when (n > cur) $ do
    put s {indent_stack = n : indents, pending_tokens = Indent:pending_tokens s}
  when (n < cur) $ do
    let i = span (> n) indents
    let (pre, post@(top : _)) = i
    if top == n
      then
        put
          s
            { indent_stack = post,
              pending_tokens = pre >>= const [Dedent, NewLine]
            }
      else error $ "Indents don't match ( " ++ show top ++ " vs " ++ show n ++ ")" ++ show s
  return $ Just NewLine
-- The functions that must be provided to Alex's basic interface

-- The input: last character, unused bytes, remaining string
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte ai =
  case ai_bytes ai of
    (b : bs) ->
      Just (b, ai {ai_bytes = bs})
    [] ->
      case ai_rest ai of
        [] -> Nothing
        (char : chars) ->
          let n = ai_line_number ai
              n' = if char == '\n' then n + 1 else n
              c = ai_column_number ai
              c' = if char == '\n' then 1 else c + 1
              (b : bs) = encode [char]
           in Just
                ( b,
                  AlexInput
                    { ai_prev = char,
                      ai_bytes = bs,
                      ai_rest = chars,
                      ai_line_number = n',
                      ai_column_number = c'
                    }
                )

getLineNo :: P Int
getLineNo = do
  s <- get
  return . ai_line_number . input $ s

getColNo :: P Int
getColNo = do
  s <- get
  return . ai_column_number . input $ s

evalP :: P a -> String -> a
evalP m s = evalState m (initialState s)

getPosition :: Int -> P TokPosition
getPosition tokenLength = do
  ParseState { input = AlexInput { ai_line_number = ln, ai_column_number = cn } } <- get
  return $ TokPosition { line = ln, column = cn - tokenLength }

