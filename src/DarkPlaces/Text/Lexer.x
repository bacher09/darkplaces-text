{
{-# OPTIONS_GHC -w #-}
module DarkPlaces.Text.Lexer (
    parseDPText
) where
import DarkPlaces.Text.Types
import qualified Data.ByteString.Lazy as BL
}

%wrapper "basic-bytestring"

$num = [0-9]
$hexnum = [$num A-Fa-f]
$newline = \n
@simple_color = \^ $num
@hex_color = \^x $hexnum{3}
@other = [^\^]+ | [. $white]

words :-

    $newline           { const DPNewline }
    @simple_color      { simpleColor }
    @hex_color         { hexColor }
    @other             { DPString }

{


-- | convert lazy `BL.ByteString` to `BinaryDPText`
parseDPText :: BL.ByteString -> BinaryDPText
parseDPText = DPText . alexScanTokens
}
