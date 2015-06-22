{-# LANGUAGE RankNTypes #-}
module DarkPlaces.Text (
    -- types
    DPTextToken(..),
    DecodeType(..),
    -- type synonyms
    DPTokenWithRange,
    DPTextOutput,
    DPTextFilter,
    -- newtypes
    BinDPText(..),
    DPText(..),
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
    -- convert funcs
    fromBinDPText,
    fromDPText,
    fromByteString,
    -- util funcs
    concatText,
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
import Control.Monad (when, join)
import Data.String
import System.IO (Handle, stdout, hPutChar)
import Control.Monad.IO.Class
import qualified Data.ByteString.UTF8 as BU
import Data.Monoid


type DPTokenWithRange a = (PositionRange, DPTextToken a)
type DPTextOutput a m = (MonadIO m) => Consumer (DPTextToken a) m ()
type DPTextFilter a m b = (Monad m) => Conduit (DPTextToken a) m (DPTextToken b)
newtype BinDPText = BinDPText [DPTextToken B.ByteString]
    deriving (Show, Eq)

newtype DPText = DPText [DPTextToken T.Text]
    deriving (Show, Eq)


instance IsString BinDPText where
    fromString s = BinDPText $ join $ stream $$ CL.consume
      where
        stream = fromByteString $ BU.fromString s


instance Monoid BinDPText where
    mempty = BinDPText []
    (BinDPText a) `mappend` (BinDPText b) = BinDPText $ a <> b


instance IsString DPText where
    fromString s = DPText $ join $ stream $$ CL.consume
      where
        mapDecode = CL.map $ mapTextToken (decode Utf8Lenient)
        stream = fromByteString (BU.fromString s) =$= mapDecode


instance Monoid DPText where
    mempty = DPText []
    (DPText a) `mappend` (DPText b) = DPText $ a <> b


conduitDPText :: (MonadThrow m) => Conduit B.ByteString m (DPTokenWithRange B.ByteString)
conduitDPText = conduitParser dptextToken


withoutRange :: (Monad m) => Conduit (DPTokenWithRange a) m (DPTextToken a)
withoutRange = CL.map snd


parseDPText :: (MonadThrow m) => Conduit B.ByteString m (DPTextToken B.ByteString)
parseDPText = conduitDPText =$= withoutRange


fromBinDPText :: (Monad m) => BinDPText -> Producer m (DPTextToken B.ByteString)
fromBinDPText (BinDPText lst) = CL.sourceList lst


fromDPText :: (Monad m) => DPText -> Producer m (DPTextToken T.Text)
fromDPText (DPText lst) = CL.sourceList lst


fromByteString :: (MonadThrow m) => B.ByteString -> Producer m (DPTextToken B.ByteString)
fromByteString bs = CL.sourceList [bs] =$= parseDPText


stripColors :: DPTextFilter a m a
stripColors = CL.filter isTextData


removeUnnecessaryColors :: DPTextFilter a m a
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


minimizeColorsFrom :: (Eq a) => DPTextToken a -> DPTextFilter a m a
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


minimizeColors :: (Eq a) => DPTextFilter a m a
minimizeColors = minimizeColorsFrom (SimpleColor 0)


toText :: (IsString a, Monad m) => Conduit (DPTextToken a) m a
toText = CL.map tokenToText


concatText :: (Monoid a, Monad m) => Consumer a m a
concatText = mconcat `fmap` CL.consume


simplifyColors :: DPTextFilter a m a
simplifyColors = CL.map convert
  where
    convert (HexColor h) = SimpleColor (simplifyColor h)
    convert x = x


toUTF :: DecodeType -> DPTextFilter B.ByteString m T.Text
toUTF dec_type = CL.map $ mapTextToken decodeFun
  where
    decodeFun = decodeQFontUTF (dec_type /= NexuizDecode) . decode dec_type


toASCII :: DecodeType -> DPTextFilter B.ByteString m T.Text
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
hOutputNoColors h = stripColors =$= CL.mapM_  (liftIO . hPutDPTextTokenPlain h)


outputNoColors :: (Printable a) => DPTextOutput a m
outputNoColors = hOutputNoColors stdout


hWithLn :: Handle -> DPTextOutput a m -> DPTextOutput a m
hWithLn h consumer = addCleanup (const newline) consumer
  where
    newline = liftIO $ hPutChar h '\n'


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
