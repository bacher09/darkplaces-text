{-# LANGUAGE OverloadedStrings, FlexibleInstances, RankNTypes #-}
module DarkPlaces.TextSpec (
    spec
) where
import Test.Hspec
import Test.QuickCheck
import DarkPlaces.Text
import DarkPlaces.Text.Types
import Control.Applicative
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Word
import Data.Char (toLower)
import Data.Conduit
import qualified Data.Conduit.List as CL
import Control.Monad (join)
import Data.Monoid
import Data.Functor.Identity


instance Arbitrary B.ByteString where
    arbitrary = B.pack <$> arbitrary


instance Arbitrary (DPTextToken B.ByteString) where
    arbitrary = do
        t <- choose (0, 3) :: Gen Int
        case t of
            0 -> SimpleColor <$> choose (0, 9)
            1 -> HexColor <$> choose (0, 0xFFF)
            2 -> DPString <$> arTextTocken
            3 -> return DPNewline


instance Arbitrary BinDPText where
    arbitrary = BinDPText <$> arbitrary


arTextTocken :: Gen B.ByteString
arTextTocken = B.pack <$> listOf1 wCarpet
  where
    wCarpet :: Gen Word8
    wCarpet = (\i -> if i >= 94 then i + 1 else i) <$> choose(0, 254)


spec :: Spec
spec = do
    describe "parseDPText" $ do
        it "test base examples" $ do
            let res = DPText [
                      SimpleColor 1,DPString "one",SimpleColor 2,
                      DPString "two",HexColor 4095,DPString "three",
                      DPNewline,DPString "four"]

            "^1one^2two^xfffthree\nfour" `shouldBe` res

        it "parsing string and converting back to string give same str" $
            property $ \xs -> lower (repr $ fromByteString xs) == lower xs

        it "converting to string and parsing gives same result" $ property $
            \xs -> repr (fromByteString ( repr $ fromBinDPText xs)) == repr (fromBinDPText xs)

    describe "stripColors" $ do
        it "should remove colors tokens" $ property $
            \xs -> not $ any isColor (concat $ fromBinDPText xs =$= stripColors $$ CL.consume)

        it "for \"^1test ^2fast\" should return \"test fast\"" $ do
            let res = toBin $ fromBinDPText "^1test ^2fast" =$= stripColors
            res `shouldBe` "test fast"

    describe "minimizeColors" $ do
        it "should reduce number of colors" $ do
            let res = toBin $ fromBinDPText "^0^1^2test hah^7lala\n^0hello" =$= minimizeColors
            res `shouldBe` "^2test hah^7lala\nhello"

    describe "simplifyColors" $ do
        it "should simplify ^x000 to ^0" $ do
            let res = toBin $ fromBinDPText "^x000Haha" =$= simplifyColors
            res `shouldBe` "^0Haha"
  where
    lower = BC.map toLower
    repr dpcon = mconcat $ dpcon =$= toText $$ concatText
    toBin :: Producer Identity (DPTextToken B.ByteString) -> BinDPText
    toBin v = runIdentity $ toBinDPText v
