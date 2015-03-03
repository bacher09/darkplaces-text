module DarkPlaces.Text where
import DarkPlaces.Text.Lexer
import DarkPlaces.Text.Types
import DarkPlaces.Text.Colors
import DarkPlaces.Text.Chars
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import System.IO (Handle, stdout, hPutStrLn)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.String


-- | Removes colors from `DPText a`
stripColors :: DPText a -> DPText a
stripColors (DPText t) = DPText $ filter isString t


minimizeColors :: (Eq a) => DPText a -> DPText a
minimizeColors (DPText t) = DPText $ minimize' t (SimpleColor 0)
  where
    minimize' (x:xs) c
        | isColor x && x == c = minimize' xs c
        | isColor x = x : minimize' xs x
        | otherwise = x : minimize' xs c

    minimize' [] _ = []


simplifyColors :: DPText a -> DPText a
simplifyColors (DPText t) =  DPText $ map convert t
  where
    convert (HexColor h) = SimpleColor (simplifyColor h)
    convert x = x


{-printColors :: Handle -> DPText -> IO ()-}
{-printColors h = printColors' h . minimizeColors . simplifyColors-}


{-hPutStrUtf :: Handle -> DPText -> IO ()-}
{-hPutStrUtf h t = printColors h (decodeDPTextUTF t) >> hSetSGR h [Reset]-}


{-hPutStrUtfNoColors :: Handle -> DPText -> IO ()-}
{-hPutStrUtfNoColors h t = printColors' h $ decodeDPTextUTF $ stripColors t-}


{-hPutStrLnUtf :: Handle -> DPText -> IO ()-}
{-hPutStrLnUtf h t = hPutStrUtf h t >> hPutStrLn h ""-}

{--- | prints `DPText` to console using utf8 encoding-}
{-putStrUtf :: DPText -> IO ()-}
{-putStrUtf = hPutStrUtf stdout-}

{--- | same as `putStrUtf` but with newline break at the end-}
{-putStrLnUtf :: DPText -> IO ()-}
{-putStrLnUtf = hPutStrLnUtf stdout-}

{--- | Will print color message if first arg is True-}
{--- | or if handle is terminal device-}
{-hPrintDPText :: Handle -> Bool -> BL.ByteString -> IO ()-}
{-hPrintDPText handle color text = case color of-}
    {-True -> hPutStrUtf handle dptext-}
    {-False -> do-}
        {-is_term <- hSupportsANSI handle-}
        {-if is_term-}
            {-then hPutStrUtf handle dptext-}
            {-else hPutStrUtfNoColors handle dptext-}
  {-where-}
    {-dptext = parseDPText text-}


{-printDPText :: Bool -> BL.ByteString -> IO ()-}
{-printDPText = hPrintDPText stdout-}

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
