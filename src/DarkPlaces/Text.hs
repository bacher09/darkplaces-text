module DarkPlaces.Text where
import DarkPlaces.Text.Lexer
import DarkPlaces.Text.Types
import DarkPlaces.Text.Colors
import DarkPlaces.Text.Chars
import DarkPlaces.Text.Classes
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import System.IO (Handle, stdout, hPutStrLn)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as BLU
import System.Console.ANSI (hSupportsANSI)
import Data.String
import Data.Monoid


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


hPutStrUtf :: (Printable a, Eq a, CharMap a) => Handle -> DPText a -> IO ()
hPutStrUtf h t = printColors h (decodeDPTextUTF t) >> hReset h


hPutStrUtfNoColors :: (Printable a, Eq a, CharMap a) => Handle -> DPText a -> IO ()
hPutStrUtfNoColors h t = hPutPrintable h $ decodeDPTextUTF $ stripColors t


hPutStrLnUtf :: (Printable a, Eq a, CharMap a) => Handle -> DPText a -> IO ()
hPutStrLnUtf h t = hPutStrUtf h t >> hPutStrLn h ""

-- | prints `DPText` to console using utf8 encoding
putStrUtf :: (Printable a, Eq a, CharMap a) => DPText a -> IO ()
putStrUtf = hPutStrUtf stdout

-- | same as `putStrUtf` but with newline break at the end
putStrLnUtf :: (Printable a, Eq a, CharMap a) => DPText a -> IO ()
putStrLnUtf = hPutStrLnUtf stdout

-- | Will print color message if first arg is True
-- | or if handle is terminal device
hPrintDPText :: Handle -> Bool -> BL.ByteString -> IO ()
hPrintDPText handle color text = case color of
    True -> hPutStrUtf handle dptext
    False -> do
        is_term <- hSupportsANSI handle
        if is_term
            then hPutStrUtf handle dptext
            else hPutStrUtfNoColors handle dptext
  where
    dptext = decodeDPText Utf8Lenient $ parseDPText text


printDPText :: Bool -> BL.ByteString -> IO ()
printDPText = hPrintDPText stdout


instance IsString (DPText BL.ByteString) where
    fromString = parseDPText . BLU.fromString


decodeDPText :: DecodeType -> BinaryDPText -> DecodedDPText
decodeDPText dec_type = mapDPText (decodeFun dec_type . BL.toStrict)
  where
    decodeFun Utf8Lenient = TE.decodeUtf8With TEE.lenientDecode
    decodeFun Utf8Ignore = TE.decodeUtf8With TEE.ignore
    decodeFun Utf8Strict = TE.decodeUtf8With TEE.strictDecode
    decodeFun NexuizDecode = TE.decodeLatin1
    decodeFun (CustomDecode f) = f
