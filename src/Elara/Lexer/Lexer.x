{
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE NoStrict #-}
module Elara.Lexer.Lexer where
import Elara.Lexer.Token
import Elara.Lexer.Utils
import Relude.Unsafe (read)
import Control.Lens ((^.), over)
import Elara.AST.Region
import Data.Text qualified as T
import Elara.Lexer.Char
import Print
import Polysemy.State
}


$white_no_nl = $white # \n 


-- Numeric Literals
$digit = 0-9
$hexit = [0-9 a-f A-F]
$octit = [0-7]
@decimal     = $digit+
@octal       = $octit+
@hexadecimal = $hexit+
@exponent    = [eE] [\-\+]? @decimal

-- Identifiers
$lower = [a-z]
$upper = [A-Z]

$opChar = [\! \# \$ \% \& \* \+ \/ \\ \< \> \? \@ \^ \| \- \~ \= \.]
$underscore = \_
$identifier = [$lower $upper $digit $underscore]

@variableIdentifer = [$lower $underscore] $identifier*
@typeIdentifier = $upper $identifier*
@opIdentifier = $opChar+

@qual = (@typeIdentifier \.)+
@qVariableIdentifer = @qual @variableIdentifer
@qTypeIdentifier = @qual @typeIdentifier
@qOpIdentifier = @qual @opIdentifier

-- Operators are a little tricky because if we allow them to start with dots, we end up with `a Prelude.+ b`
-- ending up being lexed as `(a Prelude) .+ b` instead of `a (Prelude.+ b)` (parentheses added for clarity)
-- So we have to disallow dots at the start and fix the discrepancy in the parser




-- Escapes
$escapeCode = [abfnrtv \\ \" \']
$cntrl   = [$upper \@\[\\\]\^\_]
@ascii   = \^ $cntrl | NUL | SOH | STX | ETX | EOT | ENQ | ACK
         | BEL | BS | HT | LF | VT | FF | CR | SO | SI | DLE
         | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN | EM
         | SUB | ESC | FS | GS | RS | US | SP | DEL

@ampEscape  = (\\\&)+ -- I wish I knew why this has to be handled specially
@escape      = \\ ($escapeCode | @ascii | @decimal | o @octal | x @hexadecimal)


tokens :-
  <stringSC> {
      \"                     { endString }
      @ampEscape .           { appendToStringWith translateEscapedChar }
      @escape                { appendToStringWith translateEscapedChar }
      .                      { appendToString }
  }
  <0> {
      \;                     { simpleTok TokenSemicolon }
      $white_no_nl+          ;
      \n$white*              { startWhite }
      "--" [\ ] .*  			   ;

      -- Qualified identifiers have higher precedence than the symbol tokens
      @qVariableIdentifer    { parametrizedTok TokenQVariableIdentifier splitQualName }
      @qTypeIdentifier       { parametrizedTok TokenQConstructorIdentifier splitQualName }
      @qOpIdentifier         { parametrizedTok TokenQOperatorIdentifier splitQualName }
      
      -- Literals
      \-? (
          @decimal 
          | 0[o0] @octal
          | 0[xX] @hexadecimal
          )
      { parametrizedTok TokenInt (read . toString) }

      \-? (  @decimal \. @decimal @exponent?
             | @decimal @exponent
          )
      { parametrizedTok TokenFloat (read . toString) }

      \' (. # [\'\\] | " " | @escape) \' { parametrizedTok TokenChar (read . toString) }

      -- Keywords
      let					           { simpleTok TokenLet }
      def                    { simpleTok TokenDef }
      if                     { simpleTok TokenIf }
      then                   { simpleTok TokenThen }
      else                   { simpleTok TokenElse }
      in					           { simpleTok TokenIn }
      match                  { simpleTok TokenMatch }
      with                   { simpleTok TokenWith }
      data                   { simpleTok TokenData }
      class                  { simpleTok TokenClass }
      type                   { simpleTok TokenType }
      alias                  { simpleTok TokenAlias }
      module                 { simpleTok TokenModule } 
      import                 { simpleTok TokenImport }
      infixl                 { simpleTok TokenInfixL }
      infixr                 { simpleTok TokenInfixR }
      infix                  { simpleTok TokenInfix }

      -- Symbols
      \;                     { simpleTok TokenSemicolon }
      \,                     { simpleTok TokenComma }
      \.                     { simpleTok TokenDot }
      \:                     { simpleTok TokenColon }
      \:\:                   { simpleTok TokenDoubleColon }
      \\                     { simpleTok TokenBackslash }
      \-\>                   { simpleTok TokenRightArrow }
      \<\-                   { simpleTok TokenLeftArrow }
      \=\>                   { simpleTok TokenDoubleRightArrow }
      \@                     { simpleTok TokenAt }
      \(                     { simpleTok TokenLeftParen }
      \)                     { simpleTok TokenRightParen }
      \[                     { simpleTok TokenLeftBracket }
      \]                     { simpleTok TokenRightBracket }
      \{                     { simpleTok TokenLeftBrace }
      \}                     { simpleTok TokenRightBrace }
      \`                     { simpleTok TokenBacktick }
      \|                     { simpleTok TokenPipe }
      $underscore            { simpleTok TokenUnderscore }
      \=                     { simpleTok TokenEquals }
     

      \"                     { beginString }

      -- Identifiers
      @variableIdentifer     { parametrizedTok TokenVariableIdentifier identity}
      @typeIdentifier        { parametrizedTok TokenConstructorIdentifier identity}
      @opIdentifier          { parametrizedTok TokenOperatorIdentifier identity}
  
  }

{

type NumberOfCharsMatched = Int
type MatchedSequence = Text
type LexAction = NumberOfCharsMatched -> MatchedSequence -> LexMonad (Maybe Lexeme)

simpleTok :: Token -> LexAction
simpleTok t len _ = do
  start <- getPosition len
  region <- createRegionStartingAt start 
  return $ Just (Located (RealSourceRegion region) t)

parametrizedTok :: (a -> Token) -> (Text -> a) -> LexAction
parametrizedTok tc read' tokenLen matched = do
  start <- getPosition tokenLen 
  region <- createRegionStartingAt start 
  let token = tc (read' matched)
  return $ Just (Located (RealSourceRegion region) token)






beginString :: LexAction
beginString len _ = do
  s <- get
  pos <- getPosition len
  put s{ _lexSC = stringSC, _pendingPosition = pos }
  pure Nothing

appendToString :: LexAction
appendToString = appendToStringWith T.head

appendToStringWith :: (Text -> Char) -> LexAction
appendToStringWith f len inp = do
  modify $ over stringBuf (f (T.take len inp) : )
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
      { _lexSC = 0,
        _stringBuf = ""
      }
  let token = TokenString (toText $ reverse buf)
  pure $ Just (Located (RealSourceRegion region) token)
}