{
module Elara.Lexer.Lexer where

import Elara.Lexer.Token
import Data.List.NonEmpty ((<|))
import Elara.AST.Region
import Control.Lens ((^.))

import Prelude hiding (ByteString)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as BS
import Relude.Unsafe (read)
}

%wrapper "monadUserState-bytestring"

$whitechar = [\t\n\r\v\f\ ]
$special   = [\(\)\,\;\[\]\{\}]

$digit = [0-9]
$hexit = [0-9a-fA-F]
$octit = [0-7]


$lower = [a-z]
$upper = [A-Z]
$identifierChar = [$lower $upper $digit]


@reservedid = match|class|data|default|type|if|else|then|let|in

@reservedOp = ".." | ":" | "::" | "="

@varid  = $lower $identifierChar*
@conid  = $upper $identifierChar*

@decimal     = $digit+
@octal       = $octit+
@hexadecimal = $hexit+
@exponent    = [eE] [\-\+] @decimal

tokens :-
    <0> \; { emptyTok TokenSemicolon }
    <0> let { emptyTok TokenLet }
    <0> $digit+ { parametrizedTok TokenInt (read . toString) }
    <0> $white+ { skip }

{
type AlexUserState = FilePath

alexInitUserState :: AlexUserState
alexInitUserState = ""

alexGetFilename :: Alex FilePath
alexGetFilename = Alex $ \s -> Right (s, alex_ust s)

alexGetPos :: Alex AlexPosn
alexGetPos = Alex $ \s -> Right (s, alex_pos s)

alexInitFilename :: String -> Alex ()
alexInitFilename fname = Alex $ \s -> Right (s { alex_ust = fname }, ())


alexEOF = return $ Located undefined TokenEOF

type Lexeme = Located Token

emptyTok = mkL . const

parametrizedTok :: (a -> Token) -> (Text -> a) -> AlexInput -> Int64 -> Alex Lexeme
parametrizedTok toktype f = mkL (toktype . f)

mkL :: (Text -> Token) -> AlexInput -> Int64 -> Alex Lexeme
mkL toktype (alexStartPos,_,str,_) len = do
    fname <- alexGetFilename
    alexEndPos <- alexGetPos
    let AlexPn _ startLine startCol = alexStartPos
        AlexPn _ endLine endCol = alexEndPos
        startPos = Position startLine startCol
        endPos   = Position endLine endCol
        srcSpan  = SourceRegion (Just fname) startPos endPos
        src = BS.take len str
        srcAsText = decodeUtf8 src
    pure $ Located (RealSourceRegion srcSpan) (toktype srcAsText)


lex :: FilePath -> ByteString -> Either String [Lexeme]
lex fname input = runAlex input $ alexInitFilename fname >> init <$> alexLex

alexLex :: Alex (NonEmpty Lexeme)
alexLex = do 
    lexeme <- alexMonadScan
    if lexeme ^. unlocated == TokenEOF
    then pure (lexeme :| [])
    else (lexeme <|) <$> alexLex
}