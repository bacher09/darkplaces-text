module DarkPlaces.Text.Types (
    -- types
    DPTextToken(..),
    DecodeType(..),
    -- type aliases
    BinDPTextToken,
    -- funcs
    isString,
    isColor,
    isNewline,
    isTextData,
    tokenToText,
    mapTextToken,
    decode
) where
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Text.Printf
import Data.String


data DPTextToken a = SimpleColor Int
                   | HexColor Int
                   | DPNewline
                   | DPString a
    deriving(Show, Eq)


type BinDPTextToken = DPTextToken B.ByteString


data DecodeType = Utf8Lenient
                | Utf8Ignore
                | Utf8Strict
                | NexuizDecode
    deriving(Show, Read, Eq, Ord, Enum, Bounded)


isString :: DPTextToken a -> Bool
isString (DPString _) = True
isString _ = False


isColor :: DPTextToken a -> Bool
isColor (SimpleColor _) = True
isColor (HexColor _) = True
isColor _ = False


isNewline :: DPTextToken a -> Bool
isNewline DPNewline = True
isNewline _ = False


isTextData :: DPTextToken a -> Bool
isTextData = not . isColor


tokenToText :: (IsString a) => DPTextToken a -> a
tokenToText DPNewline = fromString "\n"
tokenToText (DPString s) = s
tokenToText (SimpleColor c) = fromString $ "^" ++ show c
tokenToText (HexColor c) = fromString $ printf "^x%03X" c


mapTextToken :: (a -> b) -> DPTextToken a -> DPTextToken b
mapTextToken f (DPString s) = DPString $ f s
mapTextToken _ DPNewline = DPNewline
mapTextToken _ (SimpleColor c) = SimpleColor c
mapTextToken _ (HexColor c) = HexColor c


decode :: DecodeType -> B.ByteString -> T.Text
decode Utf8Lenient = TE.decodeUtf8With TEE.lenientDecode
decode Utf8Ignore = TE.decodeUtf8With TEE.ignore
decode Utf8Strict = TE.decodeUtf8With TEE.strictDecode
decode NexuizDecode = TE.decodeLatin1
