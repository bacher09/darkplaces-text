module DarkPlaces.Text.Types where
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text as T
import DarkPlaces.Text.Classes
import DarkPlaces.Text.Colors
import System.Console.ANSI
import System.IO (Handle)
import Data.Monoid
import Data.String
import Numeric


data DPTextToken a = SimpleColor Int
                   | HexColor Int
                   | DPString a
    deriving(Show, Eq)


newtype DPText a = DPText [DPTextToken a]
    deriving(Show, Eq)


type BinaryDPText = DPText BL.ByteString
type DecodedDPText = DPText T.Text


data DecodeType = Utf8Lenient
                | Utf8Ignore
                | Utf8Strict
                | NexuizDecode
                | CustomDecode (B.ByteString -> T.Text)


simpleColor :: BL.ByteString -> DPTextToken a
simpleColor = SimpleColor . fst . head . readDec . BLC.unpack . BL.drop 1


hexColor :: BL.ByteString -> DPTextToken a
hexColor = HexColor . fst . head . readHex . BLC.unpack . BL.drop 2


isString :: DPTextToken a -> Bool
isString (DPString _) = True
isString _ = False


isColor :: DPTextToken a -> Bool
isColor (SimpleColor _) = True
isColor (HexColor _) = True
isColor _ = False


mapDPText :: (a -> b) -> DPText a -> DPText b
mapDPText f (DPText l) = DPText $ map fun l
  where
    fun (DPString s) = DPString $ f s
    fun (SimpleColor c) = SimpleColor c
    fun (HexColor c) = HexColor c


putDPText :: (Printable a) => Handle -> DPText a -> IO ()
putDPText h (DPText t) = mapM_ print t
  where
    print (SimpleColor c) = hSetSGR h (getColor c)
    print (DPString s) = hPutPrintable h s
    print _ = return ()


instance Printable a => Printable (DPText a) where
    hPutPrintable = putDPText


instance Monoid (DPText a) where
    mempty = DPText []
    mappend (DPText a) (DPText b) = DPText $ a ++ b


toText :: (Monoid a, IsString a) => DPText a -> a
toText (DPText tl) = mconcat $ map repr tl
  where
    repr (DPString s) = s
    repr (SimpleColor c) = fromString $ "^" ++ show c
    repr (HexColor c) = fromString $ "^x" ++ showHex c ""
