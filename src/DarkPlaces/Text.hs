{-# LANGUAGE RankNTypes #-}
module DarkPlaces.Text (
    -- types
    DPTextToken(..),
    DecodeType(..),
    -- type synonyms
    DPTextOutput,
    DPTextFilter,
    -- newtypes
    BinDPText(..),
    DPText(..),
    -- functions
    parseDPText,
    stripColors,
    toUTF,
    toASCII,
    toText,
    -- convert funcs
    fromBinDPText,
    fromDPText,
    fromByteString,
    toBinDPText,
    toDPText,
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
    -- low level output
    hPutDPTextTokenPlain,
    hPutDPTextTokenANSI,
    -- low level funcs
    conduitDPText,
    minimizeColorsFrom,
    minimizeColors,
    simplifyColors,
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
import Control.Monad (when, join, liftM)
import Data.String
import System.IO (Handle, stdout, hPutChar)
import Control.Monad.IO.Class
import qualified Data.ByteString.UTF8 as BU
import Data.Function (on)


type DPTextOutput a m = (MonadIO m) => forall o. ConduitT (DPTextToken a) o m ()
type DPTextFilter a m b = (Monad m) => ConduitT (DPTextToken a) (DPTextToken b) m ()
newtype BinDPText = BinDPText [DPTextToken B.ByteString]
    deriving (Show)

newtype DPText = DPText [DPTextToken T.Text]
    deriving (Show)


instance Eq BinDPText where
    (==) = (==) `on` (\(BinDPText v) -> minimizeTextTockens v)


instance IsString BinDPText where
    fromString s = BinDPText $ join $ runConduit $ stream .| CL.consume
      where
        stream = fromByteString $ BU.fromString s


instance Semigroup BinDPText where
    (BinDPText a) <> (BinDPText b) = BinDPText $ a <> b

instance Monoid BinDPText where
    mempty = BinDPText []


instance Eq DPText where
    (==) = (==) `on` (\(DPText v) -> minimizeTextTockens v)


instance IsString DPText where
    fromString s = DPText $ join $ runConduit $ stream .| CL.consume
      where
        mapDecode = CL.map $ mapTextToken (decode Utf8Lenient)
        stream = fromByteString (BU.fromString s) .| mapDecode

instance Semigroup DPText where
    (DPText a) <> (DPText b) = DPText $ a <> b

instance Monoid DPText where
    mempty = DPText []


conduitDPText :: (MonadThrow m) => ConduitT B.ByteString (PositionRange, Maybe BinDPTextToken) m ()
conduitDPText = conduitParser maybeDPTextToken


parseDPText :: (MonadThrow m) => ConduitT B.ByteString (DPTextToken B.ByteString) m ()
parseDPText = conduitDPText .| CL.mapMaybe snd


fromBinDPText :: (Monad m) => BinDPText -> forall i. ConduitT i (DPTextToken B.ByteString) m ()
fromBinDPText (BinDPText lst) = CL.sourceList lst


toBinDPText :: (Monad m) => ConduitT () (DPTextToken B.ByteString) m () -> m BinDPText
toBinDPText stream = liftM BinDPText $ runConduit $ stream .| CL.consume


fromDPText :: (Monad m) => forall i. DPText -> ConduitT i (DPTextToken T.Text) m ()
fromDPText (DPText lst) = CL.sourceList lst


toDPText :: (Monad m) => ConduitT () (DPTextToken T.Text) m ()-> m DPText
toDPText stream = liftM DPText $ runConduit $ stream .| CL.consume


fromByteString :: (MonadThrow m) => forall i. B.ByteString -> ConduitT i (DPTextToken B.ByteString) m ()
fromByteString bs = yield bs .| parseDPText


stripColors :: DPTextFilter a m a
stripColors = CL.filter isTextData


removeUnnecessaryColors :: DPTextFilter a m a
removeUnnecessaryColors = do
    m1 <- await
    case m1 of
        Just t1 | isColor t1 -> do
            m2 <- await
            case m2 of
                Just t2 | isColor t2 -> do
                    leftover t2
                    removeUnnecessaryColors
                Just t2 -> do
                    leftover t2
                    yield t1
                    removeUnnecessaryColors
                _ -> do
                    yield t1
                    removeUnnecessaryColors
        Just t1 -> do
            yield t1
            removeUnnecessaryColors
        Nothing -> return ()


minimizeColorsFrom :: (Eq a) => DPTextToken a -> DPTextFilter a m a
minimizeColorsFrom sc = removeUnnecessaryColors .| do
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


toText :: (IsString a, Monad m) => ConduitT (DPTextToken a) a m ()
toText = CL.map tokenToText


concatText :: (Monoid a, Monad m) => forall o. ConduitT a o m a
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
    DPNewline       -> hReset h >> hPutChar h '\n'
    _               -> return ()


hOutputColors :: (Printable a, Eq a) => Handle -> DPTextOutput a m
hOutputColors h = simplifyColors .| minimizeColors .| output
  where
    output = (CL.mapM_ $ liftIO . hPutDPTextTokenANSI h) >>= (const $ liftIO $ hReset h)


outputColors :: (Printable a, Eq a) => DPTextOutput a m
outputColors = hOutputColors stdout


hOutputNoColors :: (Printable a) => Handle -> DPTextOutput a m
hOutputNoColors h = stripColors .| CL.mapM_  (liftIO . hPutDPTextTokenPlain h)


outputNoColors :: (Printable a) => DPTextOutput a m
outputNoColors = hOutputNoColors stdout


hWithLn :: Handle -> DPTextOutput a m -> DPTextOutput a m
hWithLn h consumer = consumer >>= const newline
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


minimizeTextTockens :: (Monoid a) => [DPTextToken a] -> [DPTextToken a]
minimizeTextTockens ts = case ts of
    (DPString f):(DPString s):xs -> DPString (f <> s) : minimizeTextTockens xs
    x:xs                         -> x : minimizeTextTockens xs
    []                           -> []
