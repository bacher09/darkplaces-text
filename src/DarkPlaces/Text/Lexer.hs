{-# LANGUAGE OverloadedStrings #-}
module DarkPlaces.Text.Lexer (
    dptextToken,
    maybeDPTextToken
) where
import Data.Attoparsec.ByteString hiding (takeWhile1)
import Data.Attoparsec.ByteString.Char8 (isDigit_w8, char, takeWhile1)
import Control.Applicative
import DarkPlaces.Text.Types
import Data.Word
import Data.Bits (shiftL, (.|.))


isHexDigit :: Word8 -> Bool
isHexDigit w = (w >= 48 && w <= 57)  ||
               (w >= 97 && w <= 102) ||
               (w >= 65 && w <= 70)


digitInt :: Parser Int
digitInt = (\i -> fromIntegral (i - 48)) <$> satisfy isDigit_w8


hexdigitInt :: Parser Int
hexdigitInt = convert <$> satisfy isHexDigit
  where
    convert w
        | w >= 48 && w <= 57 = fromIntegral $ w - 48
        | w >= 97 = fromIntegral $ w - 87
        | otherwise = fromIntegral $ w - 55


threeHex :: Parser Int
threeHex = convert <$> hexdigitInt <*> hexdigitInt <*> hexdigitInt
  where
    convert a b c = (a `shiftL` 8) .|. (b `shiftL` 4) .|. c


color :: Parser BinDPTextToken
color = char '^' *> (hex <|> simple) <?> "color"
  where
    simple = SimpleColor <$> digitInt
    hex = char 'x' *> (HexColor <$> threeHex)


dptextToken :: Parser BinDPTextToken
dptextToken = newline <|> color <|> (other <?> "other")
  where
    newline = char '\n' *> return DPNewline
    other = caret <|> nocaret
    nocaret = DPString <$> takeWhile1 (\c -> c /= '\n' && c /= '^')
    caret = string "^^" *> return DPEscapedCaret <|> string "^" *> return (DPString "^")


maybeDPTextToken :: Parser (Maybe BinDPTextToken)
maybeDPTextToken = optional dptextToken
