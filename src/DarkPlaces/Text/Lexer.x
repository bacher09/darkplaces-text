{
module DarkPlaces.Text.Lexer (
    DPTextToken(..),
    DPText(),
    parseDPText
) where
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
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
    @other             { DPString }

{
simpleColor :: BL.ByteString -> DPTextToken
simpleColor = SimpleColor . fst . head . readDec . BLC.unpack . BL.drop 1

hexColor :: BL.ByteString -> DPTextToken
hexColor = HexColor . fst . head . readHex . BLC.unpack . BL.drop 2

data DPTextToken = SimpleColor Int
                 | HexColor Int
                 | DPString BL.ByteString
    deriving(Show, Eq)


type DPText = [DPTextToken]


parseDPText :: BL.ByteString -> DPText
parseDPText = alexScanTokens
}
