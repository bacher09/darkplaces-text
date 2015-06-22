{-# LANGUAGE RankNTypes #-}
module DarkPlaces.Text (
    -- types
    DPTextToken(..),
    DecodeType(..),
    -- functions
    conduitDPText,
    parseDPText,
    withoutRange,
    stripColors,
    minimizeColorsFrom,
    minimizeColors,
    simplifyColors,
    toUTF,
    toASCII,
    toText,
    -- output funcs
    hOutputColors,
    outputColors,
    hOutputNoColors,
    outputNoColors,
    hOutputColorsLn,
    outputColorsLn,
    hOutputNoColorsLn,
    outputNoColorsLn,
    hPutDPTextTokenPlain,
    hPutDPTextTokenANSI,
    -- check colors
    hSupportColors,
    supportColors
) where
import DarkPlaces.Text.Lexer
import DarkPlaces.Text.Types
import DarkPlaces.Text.Colors
import DarkPlaces.Text.Chars
import DarkPlaces.Text.Classes
import qualified Data.ByteString as B
import qualified Data.Text as T
import System.Console.ANSI (hSupportsANSI, hSetSGR)
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.Attoparsec (conduitParser, PositionRange)
import Control.Monad.Catch (MonadThrow)
import Control.Monad (when)
import Data.String
import System.IO (Handle, stdout, hPutChar)
import Control.Monad.IO.Class


type DPTokenWithRange a = (PositionRange, DPTextToken a)
type DPTextOutput a m = (MonadIO m, MonadThrow m) => Consumer (DPTextToken a) m ()


conduitDPText :: (MonadThrow m) => Conduit B.ByteString m (DPTokenWithRange B.ByteString)
conduitDPText = conduitParser dptextToken


withoutRange :: (MonadThrow m) => Conduit (DPTokenWithRange a) m (DPTextToken a)
withoutRange = CL.map snd


parseDPText :: (MonadThrow m) => Conduit B.ByteString m (DPTextToken B.ByteString)
parseDPText = conduitDPText =$= withoutRange


stripColors :: (MonadThrow m) => Conduit (DPTextToken a) m (DPTextToken a)
stripColors = CL.filter isTextData


removeUnnecessaryColors :: (MonadThrow m) => Conduit (DPTextToken a) m (DPTextToken a)
removeUnnecessaryColors = do
    m1 <- await
    m2 <- await
    case (m1, m2) of
        (Just t1, Nothing) -> yield t1
        (Just t1, Just t2) | isColor t1, isColor t2 -> do
            leftover t2
            removeUnnecessaryColors
        (Just t1, Just t2) -> do
            yield t1
            leftover t2
            removeUnnecessaryColors
        _   -> return ()


minimizeColorsFrom :: (Eq a, MonadThrow m) => DPTextToken a -> Conduit (DPTextToken a) m (DPTextToken a)
minimizeColorsFrom sc = removeUnnecessaryColors =$= do
    mt <- await
    case mt of
        Nothing -> return ()
        Just t -> do
            let (sc', r) = minimize sc t
            when r $ yield t
            minimizeColorsFrom sc'
  where
    resetColor = SimpleColor 0
    minimize c x
        | isColor x && x == c = (c, False)
        | isColor x = (x, True)
        | isNewline x = (resetColor, True)
        | otherwise = (c, True)


minimizeColors :: (Eq a, MonadThrow m) => Conduit (DPTextToken a) m (DPTextToken a)
minimizeColors = minimizeColorsFrom (SimpleColor 0)


toText :: (IsString a, MonadThrow m) => Conduit (DPTextToken a) m a
toText = CL.map tokenToText


simplifyColors :: (MonadThrow m) => Conduit (DPTextToken a) m (DPTextToken a)
simplifyColors = CL.map convert
  where
    convert (HexColor h) = SimpleColor (simplifyColor h)
    convert x = x


toUTF :: (MonadThrow m) => DecodeType -> Conduit (DPTextToken B.ByteString) m (DPTextToken T.Text)
toUTF dec_type = CL.map $ mapTextToken decodeFun
  where
    decodeFun = decodeQFontUTF (dec_type /= NexuizDecode) . decode dec_type


toASCII :: (MonadThrow m) => DecodeType -> Conduit (DPTextToken B.ByteString) m (DPTextToken T.Text)
toASCII dec_type = CL.map $ mapTextToken decodeFun
  where
    decodeFun = decodeQFontASCII (dec_type /= NexuizDecode) . decode dec_type


hPutDPTextTokenPlain :: (Printable a) => Handle -> DPTextToken a -> IO ()
hPutDPTextTokenPlain h t = case t of
    (DPString s) -> hPutPrintable h s
    DPNewline    -> hPutChar h '\n'
    _            -> return ()


hPutDPTextTokenANSI :: (Printable a) => Handle -> DPTextToken a -> IO ()
hPutDPTextTokenANSI h t = case t of
    (SimpleColor c) -> hSetSGR h (getColor c)
    (DPString s)    -> hPutPrintable h s
    DPNewline       -> hPutChar h '\n' >> hReset h
    _               -> return ()


hOutputColors :: (Printable a, Eq a) => Handle -> DPTextOutput a m
hOutputColors h = simplifyColors =$= minimizeColors =$= output
  where
    output = addCleanup (const $ liftIO $ hReset h) (CL.mapM_ $ liftIO . hPutDPTextTokenANSI h)


outputColors :: (Printable a, Eq a) => DPTextOutput a m
outputColors = hOutputColors stdout


hOutputNoColors :: (Printable a) => Handle -> DPTextOutput a m
hOutputNoColors h = stripColors =$= (CL.mapM_  $ liftIO . hPutDPTextTokenPlain h)


outputNoColors :: (Printable a) => DPTextOutput a m
outputNoColors = hOutputNoColors stdout


hWithLn :: Handle -> DPTextOutput a m -> DPTextOutput a m
hWithLn h consumer = addCleanup (const $ liftIO $ hPutChar h '\n') consumer


hOutputColorsLn :: (Printable a, Eq a) => Handle -> DPTextOutput a m
hOutputColorsLn h = hWithLn h (hOutputColors h)


outputColorsLn :: (Printable a, Eq a) => DPTextOutput a m
outputColorsLn = hOutputColorsLn stdout


hOutputNoColorsLn :: (Printable a) => Handle -> DPTextOutput a m
hOutputNoColorsLn h = hWithLn h (hOutputNoColors h)


outputNoColorsLn :: (Printable a) => DPTextOutput a m
outputNoColorsLn = hOutputNoColorsLn stdout


hSupportColors :: Handle -> IO Bool
hSupportColors = hSupportsANSI


supportColors :: IO Bool
supportColors = hSupportColors stdout
