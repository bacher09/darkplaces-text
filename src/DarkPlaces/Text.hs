module DarkPlaces.Text (
    DPText(..),
    DPTextToken(..),
    DecodeType(..),
    DPStreamState(..),
    BinStreamState,
    PrintStreamArgs(..),
    BinaryDPText,
    DecodedDPText,
    parseDPText,
    defaultStreamState,
    defaultPrintStreamArgs,
    stripColors,
    minimizeColors,
    simplifyColors,
    hPrintDPText,
    printDPText,
    hPrintStreamDPText,
    printStreamDPText,
    hStreamEnd,
    streamEnd,
    toUTF,
    toASCII,
    hSupportColors,
    supportColors,
    hPutDPText,
    hPutDPTextLn,
    putDPText,
    putDPTextLn
) where
import DarkPlaces.Text.Lexer
import DarkPlaces.Text.Types
import DarkPlaces.Text.Colors
import DarkPlaces.Text.Chars
import DarkPlaces.Text.Classes
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import System.IO (Handle, stdout, hPutStrLn)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as BLU
import System.Console.ANSI (hSupportsANSI)
import Data.String
import Data.Monoid


data PrintStreamArgs = PrintStreamArgs {
    withColor   :: Bool,
    streamState :: BinStreamState,
    decodeFun   :: DecodeFun BL.ByteString T.Text
}


defaultPrintStreamArgs :: PrintStreamArgs
defaultPrintStreamArgs = PrintStreamArgs True defaultStreamState (toUTF Utf8Lenient)


-- | Removes colors from `DPText a`
stripColors :: DPText a -> DPText a
stripColors (DPText t) = DPText $ filter isTextData t


minimizeColors' :: (Eq a) => DPTextToken a -> DPText a -> DPText a
minimizeColors' sc (DPText t) = DPText $ minimize' t sc
  where
    minimize' (x:xs) c
        | isColor x && x == c = minimize' xs c
        | isColor x = x : minimize' xs x
        | isNewline x = x : minimize' xs start_color
        | otherwise = x : minimize' xs c

    minimize' [] _ = []
    start_color = SimpleColor 0


minimizeColors :: (Eq a) => DPText a -> DPText a
minimizeColors = minimizeColors' (SimpleColor 0)


simplifyColors :: DPText a -> DPText a
simplifyColors (DPText t) =  DPText $ map convert t
  where
    convert (HexColor h) = SimpleColor (simplifyColor h)
    convert x = x


splitStreamDPText :: DPStreamState a -> DPText a -> (DPText a, DPStreamState a)
splitStreamDPText st (DPText t) = (\(a, b) -> (DPText a, b)) $ go t st
  where
    go [] st = ([], st)
    go [DPString s] st = ([], st {streamLeft=s})
    go (x:xs) st = let st' = if isColor x then st {streamColor=x} else st
                       (xs', st'') = go xs st'
                   in (x : xs', st'')


parseStreamDPText :: BinStreamState -> BL.ByteString -> (BinaryDPText, BinStreamState)
parseStreamDPText st bin_data = splitStreamDPText st' dp_text
  where
    dp_text = parseDPText $ streamLeft st <> bin_data
    st' = st {streamLeft=BL.empty}


printColors :: (Printable a, Eq a) => Handle -> DPText a -> IO ()
printColors h = hPutPrintable h . minimizeColors . simplifyColors


printStreamColors :: (Printable a, Eq a) => Handle -> DPStreamState a -> DPText a -> IO ()
printStreamColors h st = hPutPrintable h . minimizeColors' (streamColor st) . simplifyColors


hPutDPText :: (Printable a, Eq a) => Handle -> DPText a -> IO ()
hPutDPText h t = printColors h t >> hReset h


hPutDPTextNoColors :: (Printable a, Eq a) => Handle -> DPText a -> IO ()
hPutDPTextNoColors h t = putDPTextNoReset h $ stripColors t


hPutDPTextLn :: (Printable a, Eq a) => Handle -> DPText a -> IO ()
hPutDPTextLn h t = hPutDPText h t >> hPutStrLn h ""

-- | prints `DPText` to console using utf8 encoding
putDPText :: (Printable a, Eq a) => DPText a -> IO ()
putDPText = hPutDPText stdout

-- | same as `putStrUtf` but with newline break at the end
putDPTextLn :: (Printable a, Eq a) => DPText a -> IO ()
putDPTextLn = hPutDPTextLn stdout

-- | Will print color message if first arg is True
-- | or if handle is terminal device
hPrintDPText ::(Printable a, Eq a) => Handle -> DecodeFun BL.ByteString a -> Bool -> BL.ByteString -> IO ()
hPrintDPText handle fun color text = if color
    then hPutDPText handle dptext
    else hPutDPTextNoColors handle dptext
  where
    dptext = fun $ parseDPText text


printDPText :: (Printable a, Eq a) => DecodeFun BL.ByteString a -> Bool -> BL.ByteString -> IO ()
printDPText = hPrintDPText stdout


hPrintStreamDPText :: Handle -> PrintStreamArgs -> BL.ByteString -> IO BinStreamState
hPrintStreamDPText h (PrintStreamArgs color st fun) bin = (if color
    then printStreamColors h st_dec dptext
    else hPutDPTextNoColors h dptext) >> return st'
  where
    (bintext, st') = parseStreamDPText st bin
    dptext = fun bintext
    st_dec = mapDPTextStream (const T.empty) st


printStreamDPText :: PrintStreamArgs -> BL.ByteString -> IO BinStreamState
printStreamDPText = hPrintStreamDPText stdout


hStreamEnd :: Handle -> Bool -> BinStreamState -> IO ()
hStreamEnd h color st = if color && streamColor st /= (SimpleColor 0)
    then hReset h
    else return ()


streamEnd :: Bool -> BinStreamState -> IO ()
streamEnd = hStreamEnd stdout


instance IsString (DPText BL.ByteString) where
    fromString = parseDPText . BLU.fromString


toDecodedDPText :: DecodeType -> BinaryDPText -> DecodedDPText
toDecodedDPText dec_type = mapDPText (decodeFun dec_type . BL.toStrict)
  where
    decodeFun Utf8Lenient = TE.decodeUtf8With TEE.lenientDecode
    decodeFun Utf8Ignore = TE.decodeUtf8With TEE.ignore
    decodeFun Utf8Strict = TE.decodeUtf8With TEE.strictDecode
    decodeFun NexuizDecode = TE.decodeLatin1


toUTF :: DecodeType -> BinaryDPText -> DecodedDPText
toUTF dec_type bin_text = decodeDPTextUTF (dec_type /= NexuizDecode) dec_text
  where
    dec_text = toDecodedDPText dec_type bin_text


toASCII :: DecodeType -> BinaryDPText -> DecodedDPText
toASCII dec_type bin_text = decodeDPTextASCII (dec_type /= NexuizDecode) dec_text
  where
    dec_text = toDecodedDPText dec_type bin_text


hSupportColors :: Handle -> IO Bool
hSupportColors = hSupportsANSI


supportColors :: IO Bool
supportColors = hSupportColors stdout
