module DarkPlaces.Text (
    DPText(..),
    DPTextToken(..),
    ToText(..),
    parseDPText,
    stripColors,
    hPutStrUtf,
    hPutStrLnUtf,
    putStrUtf,
    putStrLnUtf,
) where
import DarkPlaces.Text.Lexer
import DarkPlaces.Text.Colors
import DarkPlaces.Text.Chars
import Numeric
import qualified Data.Text.Lazy as TL
import System.Console.ANSI
import qualified Data.Text.IO as TIO
import System.IO (Handle, stdout, hPutStrLn)


class ToText a where
    toText :: a -> TL.Text

instance ToText DPTextToken where
    toText (SimpleColor x) = TL.pack $ "^" ++ show x
    toText (HexColor x) = TL.pack $ "^x" ++ showHex x ""
    toText (DPString x) = TL.fromStrict x

instance ToText DPText where
    toText (DPText x) = TL.concat $ map toText x


-- | Removes colors from `DPText`
stripColors :: DPText -> DPText
stripColors (DPText t) = DPText $ filter isString t


minimizeColors :: DPText -> DPText
minimizeColors (DPText t) = DPText $ minimize' t (SimpleColor 0)
  where
    minimize' (x:xs) c
        | isColor x && x == c = minimize' xs c
        | isColor x = x : minimize' xs x
        | otherwise = x : minimize' xs c

    minimize' [] _ = []


simplifyColors :: DPText -> DPText
simplifyColors (DPText t) =  DPText $ map convert t
  where
    convert (HexColor h) = SimpleColor (simplifyColor h)
    convert x = x


printColors' :: Handle -> DPText -> IO ()
printColors' f (DPText t) = mapM_ print t
  where
    print (SimpleColor c) = hSetSGR f (getColor c)
    print (DPString s) = TIO.hPutStr f s
    print _ = return ()


printColors :: Handle -> DPText -> IO ()
printColors h = printColors' h . minimizeColors . simplifyColors


hPutStrUtf :: Handle -> DPText -> IO ()
hPutStrUtf h t = printColors h (decodeDPTextUTF t) >> hSetSGR h [Reset]


hPutStrLnUtf :: Handle -> DPText -> IO ()
hPutStrLnUtf h t = hPutStrUtf h t >> hPutStrLn h ""

-- | prints `DPText` to console using utf8 encoding
putStrUtf :: DPText -> IO ()
putStrUtf = hPutStrUtf stdout

-- | same as `putStrUtf` but with newline break at the end
putStrLnUtf :: DPText -> IO ()
putStrLnUtf = hPutStrLnUtf stdout
