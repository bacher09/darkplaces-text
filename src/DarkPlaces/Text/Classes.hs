{-# LANGUAGE FlexibleInstances #-}
module DarkPlaces.Text.Classes (
    Printable(..),
    CharMap(..)
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
    putPrintable = hPutPrintable stdout

    putPrintableLn :: a -> IO ()
    putPrintableLn = hPutPrintableLn stdout


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


class CharMap a where
    mapChars :: (Char -> Char) -> a -> a


instance CharMap [Char] where
    mapChars = map


instance CharMap BC.ByteString where
    mapChars = BC.map


instance CharMap BLC.ByteString where
    mapChars = BLC.map


instance CharMap T.Text where
    mapChars = T.map


instance CharMap TL.Text where
    mapChars = TL.map
