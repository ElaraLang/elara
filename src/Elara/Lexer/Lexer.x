{
module Elara.Lexer.Lexer where

import Elara.Lexer.Token
import Data.List.NonEmpty ((<|))
}

%wrapper "monadUserState"

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
    <0> \; { mkL TokenSemicolon }

{
type AlexUserState = String
alexInitUserState :: AlexUserState
alexInitUserState = ""

data Lexeme = L AlexPosn TokenType String deriving Eq

alexInitFilename :: String -> Alex ()
alexInitFilename fname = Alex $ \s -> Right (s { alex_ust = fname }, ())

mkL :: TokenType -> AlexInput -> Int -> Alex Lexeme
mkL tok (p,_,_,str) len = return $ L p tok (take len str)

alexEOF = return $ L undefined TokenEOF ""

alexGetFilename :: Alex String
alexGetFilename = Alex $ \s -> Right (s, alex_ust s)

lex :: String -> String -> Either String [Lexeme]
lex fname input = runAlex input $ alexInitFilename fname >> init <$> alexLex

alexLex :: Alex (NonEmpty Lexeme)
alexLex = do 
    lexeme@(L _ tok _) <- alexMonadScan
    if tok == TokenEOF
    then pure (lexeme :| [])
    else (lexeme <|) <$> alexLex
}