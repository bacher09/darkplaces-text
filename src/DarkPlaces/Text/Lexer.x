{
{-# OPTIONS_GHC -w #-}
module DarkPlaces.Text.Lexer (
    DPTextToken(..),
    DPText(..),
    parseDPText,
    isString,
    isColor
) where
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Encoding
import qualified Data.Text.Lazy.Encoding as  TLE
import Data.String
import Numeric
}

%wrapper "basic-bytestring"

$num = [0-9]
$hexnum = [$num A-Fa-f]
@simple_color = \^ $num
@hex_color = \^x $hexnum{3}
@other = [^\^]+ | [. $white]

words :-

    @simple_color      { simpleColor }
    @hex_color         { hexColor }
    @other             { DPString . decodeUtf8 . BL.toStrict }

{
data DPTextToken = SimpleColor Int
                 | HexColor Int
                 | DPString T.Text
    deriving(Show, Eq)


newtype DPText = DPText [DPTextToken]
    deriving(Show, Eq)


simpleColor :: BL.ByteString -> DPTextToken
simpleColor = SimpleColor . fst . head . readDec . BLC.unpack . BL.drop 1

hexColor :: BL.ByteString -> DPTextToken
hexColor = HexColor . fst . head . readHex . BLC.unpack . BL.drop 2

parseDPText :: BL.ByteString -> DPText
parseDPText = DPText . alexScanTokens

isString :: DPTextToken -> Bool
isString (DPString _) = True
isString _ = False

isColor :: DPTextToken -> Bool
isColor (SimpleColor _) = True
isColor (HexColor _) = True
isColor _ = False

instance IsString DPText where
    fromString = parseDPText . TLE.encodeUtf8 . TL.pack
}
