module Arbitrary.Literals where

import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Text.Printf
import Text.Show qualified as TS (Show (..))

newtype IntLiteral = IntLiteral {unIntLiteral :: Text}
  deriving (Show)

newtype FloatLiteral = FloatLiteral {unFloatLiteral :: Text}
  deriving (Show)

newtype StringLiteral = StringLiteral {unStringLiteral :: Text}

instance Show StringLiteral where
  show = toString . unStringLiteral

newtype CharLiteral = CharLiteral {unCharLiteral :: Text}
  deriving (Show)

genInteger :: Gen Integer
genInteger = Gen.integral_ (Range.linear (-100000000) 100000000)

genDouble :: Gen Double
genDouble = Gen.realFrac_ (Range.linearFracFrom 0 (-100000000) 100000000)

genHexString :: Gen Text
genHexString = Gen.text (Range.linear 1 10) (Gen.choice hexChars)
  where
    hexChars = pure <$> ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f']

arbitraryOctString :: Gen Text
arbitraryOctString = Gen.text (Range.linear 1 10) (Gen.choice octChars)
  where
    octChars = pure <$> ['0', '1', '2', '3', '4', '5', '6', '7']

genIntLiteral :: Gen IntLiteral
genIntLiteral = IntLiteral <$> Gen.choice [genDecimal, genHex, genOct]
  where
    genDecimal = show <$> genInteger
    genHex = ("0x" <>) <$> genHexString
    genOct = ("0o" <>) <$> arbitraryOctString

genFloatLiteral :: Gen FloatLiteral
genFloatLiteral = FloatLiteral <$> Gen.choice [normalFloat, scientificIntFloat, scientificFloat]
  where
    normalFloat = show <$> Gen.double (Range.linearFracFrom 0 (-100000000) 100000000)
    scientificIntFloat = do
      p1 <- genInteger
      p2 <- Gen.integral_ (Range.linear (0 :: Integer) 100000000)
      p3 <- genInteger

      pure (safelyPrintInt p1 <> "." <> show p2 <> "e" <> show p3)

    scientificFloat = do
      p1 <- genInteger
      p2 <- genInteger
      pure (safelyPrintInt p1 <> "e" <> show p2)

genStringLiteral :: Gen StringLiteral
genStringLiteral = StringLiteral . show <$> Gen.list (Range.linear 0 10) Gen.unicode

genCharLiteral :: Gen CharLiteral
genCharLiteral = CharLiteral . show <$> Gen.unicode

safelyPrintInt :: Integer -> Text
safelyPrintInt d = fromString (printf "%d" d)
