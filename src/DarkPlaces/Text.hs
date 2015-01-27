module DarkPlaces.Text (
    DPText(..),
    DPTextToken(..),
    parseDPText,
    putStrUtf,
    putStrLnUtf,
    stripColors
) where
import DarkPlaces.Text.Lexer
import DarkPlaces.Text.Colors
import DarkPlaces.Text.Chars
import Numeric
import qualified Data.Text.Lazy as TL
import System.Console.ANSI
import qualified Data.Text.IO as TIO


class ToText a where
    toText :: a -> TL.Text

instance ToText DPTextToken where
    toText (SimpleColor x) = TL.pack $ "^" ++ show x
    toText (HexColor x) = TL.pack $ "^x" ++ showHex x ""
    toText (DPString x) = TL.fromStrict x

instance ToText DPText where
    toText (DPText x) = TL.concat $ map toText x


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


printColors' :: DPText -> IO ()
printColors' (DPText t) = mapM_ print t
  where
    print (SimpleColor c) = setSGR (getColor c)
    print (DPString s) = TIO.putStr s
    print _ = return ()


printColors :: DPText -> IO ()
printColors = printColors' . minimizeColors . simplifyColors


putStrUtf :: DPText -> IO ()
putStrUtf t = printColors (decodeDPTextUTF t) >> setSGR [Reset]


putStrLnUtf :: DPText -> IO ()
putStrLnUtf t = putStrUtf t >> putStrLn ""
