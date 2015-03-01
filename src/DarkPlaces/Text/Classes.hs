module DarkPlaces.Text.Classes (
    Printable(..)
) where
import System.IO (Handle, stdout, hPutStr, hPutStrLn)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO


class Printable a where
    hPutPrintable :: Handle -> a -> IO ()

    hPutPrintableLn :: Handle -> a -> IO ()
    hPutPrintableLn h d = hPutPrintableLn h d >> hPutStrLn h ""

    putPrintable :: a -> IO ()
    putPrintable d = hPutPrintable stdout d

    putPrintableLn :: a -> IO ()
    putPrintableLn d = hPutPrintableLn stdout d


instance Printable [Char] where
    hPutPrintable = hPutStr


instance Printable BC.ByteString where
    hPutPrintable = BC.hPut
    hPutPrintableLn = BC.hPutStrLn


instance Printable BLC.ByteString where
    hPutPrintable = BLC.hPut
    hPutPrintableLn = BLC.hPutStrLn


instance Printable T.Text where
    hPutPrintable = TIO.hPutStr
    hPutPrintableLn = TIO.hPutStrLn


instance Printable TL.Text where
    hPutPrintable = TLIO.hPutStr
    hPutPrintableLn = TLIO.hPutStrLn
