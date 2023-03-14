module Elara.Parse.Literal (charLiteral, stringLiteral, integerLiteral, floatLiteral) where

import Elara.Lexer.Token (Token (TokenChar, TokenFloat, TokenInt, TokenString))
import Elara.Parse.Primitives (HParser, satisfyMap)

charLiteral :: HParser Char
charLiteral = satisfyMap $ \case
  TokenChar i -> Just i
  _ -> Nothing

stringLiteral :: HParser Text
stringLiteral = satisfyMap $ \case
  TokenString i -> Just i
  _ -> Nothing


integerLiteral :: HParser Integer
integerLiteral = satisfyMap $ \case
  TokenInt i -> Just i
  _ -> Nothing

floatLiteral :: HParser Double
floatLiteral = satisfyMap $ \case
  TokenFloat i -> Just i
  _ -> Nothing